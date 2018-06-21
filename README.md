# TTK4145: Elevator Project Written i Erlang
Authors: Simen Keiland Fondevik, Marius Rundhovde
This repo contains our solution for the elevator project in TTK4145 Real-Time Programming spring 2018. 25% of the final grade was based on the elevator project. We scored second best out of almost 100 groups taking the course this year, with the points distributed as follows on the three evaluation criterias design, code quality and functionality.

| Design review | Code review | Completion |  Total  |
| ------------- | ----------- | ---------- | ------- |
| 7.4/8         | 8.2/9       | 8/8        | 23.6/25 |

## Introduction
An elevators primary purpose is to transport people and items between floors. A sensible metric of performance could be something indicating how well and fast this primary task is done. This is not something not cared about in this project. The elevators should avoid starvation and "silly actions", other than that, not much attention is given to their performance as elevators. This project is really about faults, how they are bound to happen, and how to keep them from ruining the day. 

On the good days, the elevator system is bound to be under-appreciated. Corporate procurement officers will perhaps favor cheaper or faster alternatives. But when the day comes that an especially large earthquake simultaneously causes the largest tsunami the world has ever seen, cracks the concrete in a biological warfare lab and cause a nuclear reactor meltdown, this elevator system should be all worth it. Most of the server rooms will be flooded. Radioactive zombies will be chewing on the network cables. And most of the disks will be wiped out by the earthquake itself. In this chaotic post apocalyptic world, the only constant will be that the elevator system will remain in working order, and every order that is taken, will also be served.

## Technical Specification
The "technical specification" of the elevator system was simple but ambiguous and incomplete. It was only to be regarded as a subset of the "complete specification". However, one should be able to create a system adhering to specification just by reading the "technical specification". The technical specification can be found [here](SPECIFICATION.md)

## Design details
The only module not entirely written by us is the [color module](./src/color.erl) forked from [julianduque](https://github.com/julianduque/erlang-color) used for ANSI coloring of terminal output.

The entry point of the code is [init_node](./src/init_node.erl). This module spawns neceessary modules with links. If one of the linked processes crashes, the entire module is restarted.

Our solution uses the beauty of distributed Erlang. The [node connection](./src/node_conection.erl) module sets up a node cluster which automatically finds and conencts to other nodes by broadcasting and listening for node names.

The [hardware reader](./src/hardware_reader.erl) moule communicates withe the [driver](./src/driver_interface) and detects new floors and orders. A new order is sent to the [communication interface](./src/communication_interface.erl), which further distributes the order to all other nodes. They add the order to their [orders and states](./src/orders_and_states.erl) module and sends an acknowledge message back to the original node. This node then adds the order locally and distributes a message to all nodes to lit the order button LED. Detection of new floors is handled by the [state machine](./src/fsm.erl) module.

The [watchdog](./src/watchdog.erl) module ensure all orders will be served within reasonable time, whereas the [cost function](./src/cost_function.erl) calculates which order an elevator should serve.

Dependencies between modules can be stated as follows. Note that modules are listed with their registered process names.


| This module     | Receives msgs/function calls from            | Sends msgs/function calls to
| --------------- |----------------------------------------------| ----------------------------
| init_node       | N/A                                          | N/A                                                               
| driver          | hardware_reader, communicator, fsm           | hardware_reader (answer query)                                    
| hardware_reader | driver                                       | driver, communicator, fsm                                         
| node_connector  | fsm (upon errors only)                       | data_manager                                                      
| communicator    | hardware_reader, data_manager, fsm           | driver, data_manager                                         
| data_manager    | communicator, cost_function, watchdog        | communicator, cost_function, fsm, watchdog                        
| fsm             | hardware_reader, data_manager, watchdog      | driver, communicator, watchdog, node_connector
| watchdog        | data_manager, fsm                            | data_manager, fsm                                                 
| cost_function   | data_manager                                 | data_manager                                                      

Throughtout the project, the following color scheme for terminal printing is used:
- Red     prints indicate an error or an unexpected message/behaviour
- Cyan    prints indicate PIDs of processes spawned from this module
- Green   prints indicate a node connecting
- Magenta prints indicate timeouts and nodes disconnecting
- Yellow  prints indicate change of state in fsm
