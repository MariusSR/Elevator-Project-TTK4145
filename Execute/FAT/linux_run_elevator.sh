clear

# Start ElevatorServer
pkill ElevatorServer
~/.cargo/bin/ElevatorServer & disown


# Run init_node:start()
cd ~/Desktop/project-ssm/Execute/ebin/
gnome-terminal -x erl -s init_node start
