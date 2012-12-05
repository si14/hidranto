The problem
===========

We have a webserver that serves as a frontend for some number of backends. Backends can execute tasks. Details of tasks are known only to backends. We want to regulate the load on backends on frontend.

General architecture
====================

Task is started in 2 phases: prepare and start. First one returns special values called "backpressure triplets" that consist from 3 values: {Type, Key, Price}. Type is a type of resource that will be consumed by task. Key is an identifier of particular resource. Price is an amount of resource that will be consumed when task starts.

There is also a config, where {Type, [CreditPoolSize and/or RateLimit]} pairs are set.

Why Type and Key
================

Let's assume that backends has a number of worker pools. If some task consumes some global resource from all that pools, we can return {particular_type, '_', 1} as a backpressure triplet. If a task consumes resources from particular worker pool, we return {particular_type, particular_worker, 1} triplet. The set of possible Keys is finite and so large (it's important for understanding design decisions later in this text).

Possible config extension
=========================

We can include particular Keys (as opposed to types only) to limit load in particular case.

General architecture
====================

It makes sense to assume that we have only one queue (for beginning, at least). We scan this queue from head to tail, searching tasks that can be started now. Because the queue size is limited and constraint check is quite hard, it makes sense to add an additional layer of control: to add constraints on rate with which particular client can put items in this queue and the total number of tasks that can be executed by him (including tasks that was started and removed from the queue).
