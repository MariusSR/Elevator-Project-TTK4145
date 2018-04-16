# TTK4145: Elevator Project Written i Erlang
This repo contains our solution for the elevator project in TTK4145 Real-Time Programming spring 2018.

## For the Codereview
The only module not entirely written by us is the [color module](./src/color.erl) forked from [julianduque](https://github.com/julianduque/erlang-color). The entry point of the code is [init_node.erl](./src/init_node.erl). This module spawns neceessary modules with linkin. If one of the linked processes crashes, the entire module is restarted.

The [hardware reader](./src/hardware_reader.erl) moule detects new floors and orders. A new order is sent to the [communication interface](./src/communication_interface.erl), which further distributes the order to all other nodes. They add the order to their [orders and states](./src/orders_and_states.erl) module and sends an acknowledge message back to the original node. This node then adds the order locally and distributes a message to all nodes to lit the order button LED. Detection of new floors is handled by the [state_machine](./src/fsm.erl).


## Introduction
An elevators primary purpose is to transport people and items between floors. A sensible metric of performance could be something indicating how well and fast this primary task is done. This is not something not cared about in this project. The elevators should avoid starvation and "silly actions", other than that, not much attention is given to their performance as elevators. This project is really about faults, how they are bound to happen, and how to keep them from ruining the day. 

On the good days, the elevator system is bound to be under-appreciated. Corporate procurement officers will perhaps favor cheaper or faster alternatives. But when the day comes that an especially large earthquake simultaneously causes the largest tsunami the world has ever seen, cracks the concrete in a biological warfare lab and cause a nuclear reactor meltdown, this elevator system should be all worth it. Most of the server rooms will be flooded. Radioactive zombies will be chewing on the network cables. And most of the disks will be wiped out by the earthquake itself. In this chaotic post apocalyptic world, the only constant will be that the elevator system will remain in working order, and every order that is taken, will also be served.

## Specification
### Technical Specification
The "technical specification" of the elevator system was simple but most likely ambiguous and incomplete. It was only to be regarded as a subset of the "complete specification". However, one should be able to create a system adhering to specification just by reading the "technical specification". The technical specification can be found [here](SPECIFICATION.md)

## Evaluation
In TTK4145 (2018) 25% of the final grade was based on the elevator project. The project gave a maximum score of 25 points distributed as follows on the three evaluation criterias deisgn, code quality and functionality.

| Design review | Code review | Completion | Total |
| ------------- | ----------- | ---------- | ----- |
| 8             | 9           | 8          | 25    |

## Solution
### Language
Why Erlang.

### Design
Design description.
