# A statecharts intepreter for PostgreSQL

Statecharts is a SQL interpreter for state machines represented using [statecharts](https://statecharts.dev)
Statecharts are a powerful way to model the behavior of complex systems and applications, providing a clear and visual
representation of state transitions and actions.

## What are Statecharts?

Statecharts are a formalism for modeling the behavior of systems, particularly those with complex and hierarchical state transitions.
This standard is maintained by the World Wide Web Consortium (W3C) and has gained popularity due to its effectiveness in designing robust,
maintainable, and understandable systems.

Statecharts represent states, events, transitions, and actions in a visual manner that makes it easier to reason about system behavior.
This approach is especially useful for complex applications, where the traditional if-else or switch-case statements can quickly become
unmanageable and prone to errors.

## Why Use Statecharts?

Using Statecharts for managing state transitions in your database offers several benefits:

1. **Modularity and Readability**: Statecharts enable you to break down complex logic into smaller, manageable components,making your codebase more modular and easier to understand.
2. **Clear State Transitions**: The visual representation of state transitions helps developers and stakeholders to clearly see how the system behaves under different conditions.
3. **Hierarchical States**: Statecharts support hierarchical states, allowing you to model complex systems with multiple levels of abstraction.
4. **Event-Driven Architecture**: Statecharts follow an event-driven approach, aligning well with how modern applications handle user interactions and system events.

## Why use Statecharts directly in the database

1. **Seamless Integration**: Statecharts can be seamlessly integrated into your PostgreSQL database, leveraging the power and efficiency of your existing infrastructure.
2. **Database Logic Without Triggers**: By using Statecharts, you can write cleaner logic directly in your database without relying heavily on triggers, making your codebase more maintainable and less prone to hidden bugs.
3. **Persistent State Management**: State machines represented by Statecharts can be stored persistently in the database, ensuring that your application's state remains consistent even in the face of errors.
4. **Easy Data Manipulation**: Manipulating statecharts becomes straightforward as it involves simple insertions and updates in the designated tables, which are neatly astracted as functions.

## Getting started

The easiert way to deploy the statecharts schema and functions to your postgres database is by using [sqitch](https://sqitch.org)

```bash
git clone https://github.com/kronor-io/statecharts
cd statecharts
sqitch deploy -t postgresql://user:password@host/db_name
```

The commad above will install the funcitons in your database


