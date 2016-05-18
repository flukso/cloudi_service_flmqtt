VSN=1.5.1
SERVICE=cloudi_service_flmqtt

PATH_PREFIX=/usr/local
CORE_PATH=$(PATH_PREFIX)/lib/cloudi-$(VSN)/lib/cloudi_core-$(VSN)

HOST=localhost
PORT=6464
URL_PREFIX=http://$(HOST):$(PORT)/cloudi/api/rpc

PWD=$(shell pwd)
UUID=

compile:
	cp src/$(SERVICE).app.src ebin/$(SERVICE).app
	erlc -pz $(CORE_PATH) -pz $(CORE_PATH)/ebin -o ebin/ src/*.erl

clean:
	rm ebin/*

start:
	@curl -X POST -d '"'$(PWD)/ebin'"' $(URL_PREFIX)/code_path_add.erl
	@echo
	@curl -X POST -d @$(SERVICE).conf $(URL_PREFIX)/services_add.erl
	@echo

restart:
	@curl -X POST -d '"['$(UUID)'"]' $(URL_PREFIX)/services_restart.erl
	@echo

stop:
	@curl -X POST -d '["'$(UUID)'"]' $(URL_PREFIX)/services_remove.erl
	@echo

list:
	@curl $(URL_PREFIX)/services.erl
	@echo

help:
	@echo 'You need to have cloudi running (check with cloudi ping).'
	@echo 'Makefile targets:'
	@echo ' compile'
	@echo ' clean'
	@echo ' start'
	@echo ' restart UUID="..."'
	@echo ' stop UUID="..."'

.PHONY: compile clean start restart stop list help
