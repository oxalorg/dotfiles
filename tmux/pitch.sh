#!/bash/bin

PROJDIR=~/projects/pitch-app-2
ENTER=C-m

tmux send-keys \
	"cd $PROJDIR" \
	$ENTER \
	"pit db && services/backend/scripts/with-playground-auth pit backend -s" \
	$ENTER

tmux split-window -h -c $PROJDIR
tmux send-keys \
	-t ".right" \
	"desktop-app/scripts/with-playground-backend pit dev"
