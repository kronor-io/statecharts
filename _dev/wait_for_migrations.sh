#! /usr/bin/env bash

RED='\033[0;31m'
NC='\033[0m' # No Color

until [ ! -z "$(docker compose exec -T postgres psql -c 'select * from fsm.statechart' 2>/dev/null)" ]

do
  echo "Waiting for database boot sequence........"
  sleep 1s
done

until [ ! -z "$(docker compose exec -T postgres sqitch status 2>&1 | grep "Nothing to deploy")" ];
do
  echo "Waiting for migrations to be applied......"
  sleep 1s

  # If migrations are not automatically running, run them now
  if [ ! -z "$(docker compose exec -T postgres sqitch status 2>&1 | grep "No changes deployed")" ]
    then
      COMPOSE_INTERACTIVE_NO_CLI=1 docker compose exec postgres sqitch --chdir statecharts deploy
      COMPOSE_INTERACTIVE_NO_CLI=1 docker compose exec postgres sqitch deploy
  fi

  if [ ! -z "$(docker compose exec -T postgres sqitch status 2>&1 | grep "Cannot find this change")" ]
    then
      echo -e "${RED}"
      echo -e "##################################################################"
      echo -e "#                    Cannot run migrations                       #"
      echo -e "##################################################################"
      echo -e "${NC}"
      echo -e "The database currently has migrations applied from another branch"
      echo -e "So, I don't know how to revert the changes and apply whatever is"
      echo -e "in this branch. You have a few options:"
      echo -e ""
      echo -e "* Go to the previous branch you were in and run:"
      echo -e ""
      echo -e "       git switch [my-previous-branch]"
      echo -e "       docker-compose exec postgres sqitch revert -y"
      echo -e ""
      echo -e "or"
      echo -e ""
      echo -e "* Nuke everything and start from scratch:"
      echo -e ""
      echo -e "       docker-compose down -v postgres"
      echo -e ""
      exit 1
  fi
done

