clear

# Start ElevatorServer
cd ../Simulator_v2
./SimElevatorServer_macOS

# Compile and move *.beam files to ebin
cd ../project-ssm/src
erlc *.erl;
mkdir -p ../ebin
mv *.beam ../ebin/;

# Run init_node:start()
cd ../ebin/
# erl -s init_node start
Terminal -x erl -s init_node start
