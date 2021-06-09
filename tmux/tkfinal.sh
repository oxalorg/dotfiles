tmux split-window -h 'cd /tk/tk-final && yarn run start'
tmux split-window -h 'cd /tk/tk-final && docker-compose up'
cd /tk/tk-final
workon tk-final
python3 manage.py runserver
