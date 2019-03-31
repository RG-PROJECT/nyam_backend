PROJECT = nyam_backend
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = sync cowboy mongodb jiffy mongodb

dep_cowboy_commit = 2.6.1
dep_mongodb = git https://github.com/comtihon/mongodb-erlang.git "v3.2.0"
dep_jiffy = git https://github.com/davisp/jiffy "0.15.2"

DEP_PLUGINS = cowboy

PROJECT_APP_EXTRA_KEYS = {included_applications, ['mnesia']}

include erlang.mk
