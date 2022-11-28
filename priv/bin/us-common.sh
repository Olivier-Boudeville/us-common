# Common script facilities relating to the Universal Server.
#
# Allows to avoid code duplication. Meant to be sourced, not directly executed.
#
# Used by the services relying on us-common.

#echo "(sourcing us-common.sh)"

# Notes:
#  - "-f" detects symlinks as well
#  - some whitespace flexibility is allowed in the configuration files

# Determining us_common_root if needed:
if [ -z "${us_common_root}" ]; then

	#echo "Determining us_common_root"
	# Assuming then running a script in us_common/priv/bin:
	us_common_root=$(realpath $(dirname $0)/../..)

	#echo "us_common_root determined as '${us_common_root}'."

else

	:
	#echo "us_common_root read as '${us_common_root}'."

fi



# Sets notably: us_config_dir, us_config_file, epmd_opt, vm_cookie,
# execution_context, us_username, us_groupname, us_app_base_dir, us_log_dir.
#
# The US configuration file is searched in turn through following directories:
#  1. any command-line specified US configuration directory
#  2. any directory set in the XDG_CONFIG_HOME environment variable
#  3. in $HOME/.config
#
# For each directory candidate D, the searched configuration file is:
# '$D/universal-server/us.config'.
#
read_us_config_file()
{

	us_config_filename="us.config"

	# Use any command-line specified overriding US configuration directory (for
	# runtime basics):
	#
	us_config_dir="$1"


	if [ -n "${us_config_dir}" ]; then

		if [ -d "${us_config_dir}" ]; then

			echo "Using command-line specified US configuration directory '${us_config_dir}'."

		else

			echo " Error, command-line specified US configuration directory '${us_config_dir}' not found." 1>&2
			exit 10

		fi

		us_config_file="${us_config_dir}/${us_config_filename}"

		if [ ! -f "${us_config_file}" ]; then

			echo " Error, no US configuration file found (no '${us_config_file}')." 1>&2

			exit 15

		fi

	else

		echo "No US configuration directory specified on the command-line, searching it through standard paths."

		# Otherwise (not set on the command-line; the general case), try to use
		# the default US configuration directory for all US-related services; we
		# try to apply in this script exactly the same rules as
		# class_USConfigServer, in order to always select the same actual
		# configuration file that the US framework itself will use afterwards,
		# once launched:

		if [ -z "${XDG_CONFIG_HOME}" ]; then

			# May resolve to /root, if run through sudo:
			base_path="$HOME/.config"

		else

			base_path="${XDG_CONFIG_HOME}"

		fi

		app_dir="universal-server"

		us_config_dir="${base_path}/${app_dir}"

		us_config_file="${us_config_dir}/${us_config_filename}"

		echo "Looking up '${us_config_file}'..."

		if [ ! -f "${us_config_file}" ]; then

			if [ -z "${XDG_CONFIG_DIRS}" ]; then

				base_path="/etc/xdg"

			else

				# Pops the first element of that list:
				#
				# (note: currently the next - if any - directories in that list
				# are not tested; not implemented yet)
				#
				base_path=$(echo "${XDG_CONFIG_DIRS}" | sed 's|:.*$||1')

			fi

			us_config_dir="${base_path}/${app_dir}"
			us_config_file="${us_config_dir}/${us_config_filename}"

			echo "Looking up '${us_config_file}'..."

			if [ -f "${us_config_file}" ]; then

				echo "No configuration file specified on the command-line, using default one '${us_config_file}'."

			else

				# Any hardcoded, built-in defaults not desirable, as could
				# silently hide an error:

				#echo " Warning: no US configuration file '${us_config_file}' specified, and no default one found, using built-in defaults." 1>&2

				#erl_epmd_port=4506
				#vm_cookie="xxx"
				#execution_context="production"

				# So:
				echo " Error, no command-line specified or default US configuration file found." 1>&2

				exit 20

			fi

		fi

	fi

	# Note that us_config_file remains available to the caller:
	echo "Using US configuration file '${us_config_file}'."


	# US configuration content, read once for all, with comments (%) removed:
	us_base_content=$(/bin/cat "${us_config_file}" | sed 's|^[[:space:]]*%.*||1')

	erl_epmd_port=$(echo "${us_base_content}" | grep epmd_port | sed 's|^[[:space:]]*{[[:space:]]*epmd_port,[[:space:]]*||1' | sed 's|[[:space:]]*}.$||1')

	if [ -z "${erl_epmd_port}" ]; then

		#echo "No Erlang EPMD port specified, not interfering with context defaults."
		epmd_opt=""

	else

		#echo "Using specified EPMD port, '${erl_epmd_port}'."
		epmd_opt="ERL_EPMD_PORT=${erl_epmd_port}"

	fi

	#echo "epmd_opt = $epmd_opt"

	# Note that these shell scripts shall fetch any cookie as it is, i.e. not
	# surrounded by single quotes, otherwise it will not be accepted (ex: when
	# using erl_call).

	# Strangely unable to protect single quotes with backslash, so having to use
	# double quotes:
	#
	vm_cookie=$(echo "${us_base_content}" | grep vm_cookie | sed "s|^{[[:space:]]*vm_cookie,[[:space:]]*'||1" | sed "s|'[[:space:]]*}.$||1")

	#echo "vm_cookie = $vm_cookie"


	execution_context=$(echo "${us_base_content}" | grep execution_context | sed 's|^{[[:space:]]*execution_context,[[:space:]]*||1' | sed 's|[[:space:]]*}.$||1')

	# echo "execution_context = $execution_context"

	if [ -z "${execution_context}" ]; then

		# Default:
		execution_context="production"

	fi


	us_username=$(echo "${us_base_content}" | grep us_username | sed 's|^{[[:space:]]*us_username,[[:space:]]*||1' | sed 's|[[:space:]]*}.$||1')

	#echo "us_username = $us_username"


	us_groupname=$(echo "${us_base_content}" | grep us_groupname | sed 's|^{[[:space:]]*us_groupname,[[:space:]]"*||1' | sed 's|"[[:space:]]*}.$||1')

	#echo "us_groupname = $us_groupname"


	us_app_base_dir=$(echo "${us_base_content}" | grep us_app_base_dir | sed 's|^{[[:space:]]*us_app_base_dir,[[:space:]]*"||1' | sed 's|"[[:space:]]*}.$||1')

	if [ -n "${us_app_base_dir}" ]; then

		echo "Using specified base directory for the US application, '${us_app_base_dir}'."

	else

		# Environment variable maybe:
		if [ -n "${US_APP_BASE_DIR}" ]; then

			echo "No base directory specified for the US application, using the value of the US_APP_BASE_DIR environment variable, trying '${us_app_base_dir}'."
			us_app_base_dir="${US_APP_BASE_DIR}"

		else

			# Guessing as last-resort:

			if [ -n "${us_actual_root}" ]; then
				# Here the actual caller (a US-* specialisation, relying on
				# US-Common) redefined the actual root to use, so we rely on it:
				#
				#echo "(relying on specified US actual root '${us_actual_root}')"
				us_app_base_dir="${us_actual_root}"

			else

				# Last chance:
				#echo "(no us_actual_root set)"
				us_app_base_dir="${us_common_root}"

			fi

			echo "  Warning: no base directory specified for the US application (no 'us_app_base_dir' entry in the main US configuration file '${us_config_file}' nor US_APP_BASE_DIR environment variable set), hence guessed as '${us_app_base_dir}'." #1>&2

		fi

	fi

	if [ ! -d "${us_app_base_dir}" ]; then

		echo "  Error, the base directory determined for the US application, '${us_app_base_dir}', is not an existing directory." 1>&2
		exit 30

	fi


	# In the context of the US server, us_log_dir is the directory in which
	# VM-level and Traces log files are to be written:

	us_log_dir=$(echo "${us_base_content}" | grep us_log_dir | sed 's|^{[[:space:]]*us_log_dir,[[:space:]]*"||1' | sed 's|"[[:space:]]*}.$||1')

	if [ -z "${us_log_dir}" ]; then

		us_log_dir="${us_app_base_dir}/log"

		if [ ! -d "${us_log_dir}" ]; then

			saved_log_dir="${us_log_dir}"

			# Maybe in development mode then (i.e. as a rebar3 build tree):
			us_log_dir="${us_app_base_dir}/_build/default/rel/universal_server/log"

			if [ ! -d "${us_log_dir}" ]; then

				echo "  Warning: no US log directory found: neither '${saved_log_dir}' (as a standard release) nor '${us_log_dir}' (as a rebar3 build tree)." 1>&2
				#exit 35

			else

				echo "Rebar3 build tree detected, US log directory found as '${us_log_dir}'."
			fi

		else

			echo "Standard OTP release detected, US log directory found as '${us_log_dir}'."

		fi

	else

		# Checks whether absolute:
		if [[ "${us_log_dir:0:1}" == / || "${us_log_dir:0:2}" == ~[/a-z] ]]; then

			echo "Using directly specified directory for logs, '${us_log_dir}'."

		else

			# If it is relative, it is relative to the US application base
			# directory:
			#
			us_log_dir="$(realpath ${us_app_base_dir}/${us_log_dir})"
			echo "Using specified directory for US logs (made absolute), '${us_log_dir}'."

		fi

	fi

	if [ ! -d "${us_log_dir}" ]; then

		echo "  Warning: no US log directory found ('${us_log_dir}')." #1>&2
		# Not an error by default: exit 60

	else

		echo "US logs will be written in the '${us_log_dir}' directory."

	fi


	# To delimit between us-main and any next similar (upper) layer
	# configuration:
	#
	echo

}




secure_authbind()
{

	authbind="/bin/authbind"

	if [ ! -x "${authbind}" ]; then

		echo "  Error, authbind ('${authbind}') not found." 1>&2
		exit 60

	fi

}

#echo "(us-common.sh sourced)"
