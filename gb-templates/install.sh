#!/usr/bin/env bash
DEBUG=false
GH_SETUP=../.git/gh-setup
echo "Bridge for Git local script installer."
echo "Note: Run this command from the gb-templates/ location of the repository."
GIT_URL_REGEX="^(http(s)?)(:\/\/)((([^\/:]+)(\:)([^\/:]+)(\@))|(([^\/:]+)(\@))?)([^\/:]+)\/(.+).git$"
if [ ! -f $GH_SETUP ]; then
	############ Get username from remote url ##############
	if [ -z "$GITHUB_USER_LOGIN" ]; then
		REMOTE_URL=$(git config --get remote.origin.url)
		$DEBUG && echo "Debug:Git remote url: $REMOTE_URL"

		if [[ $REMOTE_URL =~ $GIT_URL_REGEX ]]; then
			URL_PROTOCOL=${BASH_REMATCH[1]}
		    #separator=${BASH_REMATCH[2]}
		    #https=${BASH_REMATCH[3]}
		    URL_USER=${BASH_REMATCH[6]}
		    GH_HOSTNAME=${BASH_REMATCH[13]}
			GITHUB_REPO_NAME=${BASH_REMATCH[5]}
			$DEBUG && echo "DEBUG: URL_PROTOCOL $URL_PROTOCOL HOSTNAME $GH_HOSTNAME URL_USER $URL_USER GITHUB_REPO_NAME $GITHUB_REPO_NAME"
		fi
		if [ ! -z "$URL_USER" ]; then
			$DEBUG && echo "DEBUG:Taking the username from url: $URL_USER"
			GITHUB_USER_LOGIN=$URL_USER
			echo "$GITHUB_USER_LOGIN" > "$GH_SETUP"
		fi
	fi
	########################################################
	########## Get username with git gui prompt ############
	if [ -z "$GITHUB_USER_LOGIN" ]; then
		echo "Please enter your Git server username: "
		GITHUB_USER_LOGIN=$(git gui--askpass "Please enter your Git server username:")
		if [ ! -z "$GITHUB_USER_LOGIN" ]; then
			echo "$GITHUB_USER_LOGIN" > "$GH_SETUP"
		fi
		$DEBUG && echo "DEBUG:Username read from GIT GUI: $GITHUB_USER_LOGIN"
	fi
	########################################################
	############# Get username from stdin ##################
	if [ -z "$GITHUB_USER_LOGIN" ]; then
		exec < /dev/tty
		read GITHUB_USER_LOGIN
		exec <&-
		if [ ! -z "$GITHUB_USER_LOGIN" ]; then
			echo "$GITHUB_USER_LOGIN" > "$GH_SETUP"
		fi
		$DEBUG && echo "DEBUG:Username read from STDIN: $GITHUB_USER_LOGIN"
	fi
else
	GITHUB_USER_LOGIN=$(head -n 1 $GH_SETUP)
	echo "Settings detected. Git server username is: $GITHUB_USER_LOGIN"
fi
mkdir -p ../.git/hooks
cp pre-push ../.git/hooks/pre-push
echo "Git Hook copied successfully."
