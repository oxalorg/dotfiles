#!/bash/bin

PROJDIR=~/projects/pitch-app-2
ENTER=C-m

tmux send-keys \
	"cd $PROJDIR" \
	$ENTER \
	"pit db && services/backend/scripts/with-playground-auth pit backend -s" \
	$ENTER

tmux split-window -h -c $PROJDIR
tmux split-window -v -c $PROJDIR

tmux send-keys -t ".right" \
    $ENTER \
    "cd $PROJDIR/projects/zoom-app" \
    $ENTER \
	"scripts/with-playground-backend scripts/dev" \
    $ENTER

tmux send-keys -t ".down" \
    $ENTER \
    "cd $PROJDIR/projects/zoom-app" \
    $ENTER \
    "scripts/start-tunnel" \
    $ENTER

