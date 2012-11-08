1. Benchmark property file

LoggingLevel: possible values are OFF, INFO, FINE, FINER, FINEST
LogFile: the file path relative to benchmark property file
JmsServerHostName: the host on which the JMS server is running
JmsServerPort: the port at which the JMS server is running
PublisherQueue: the name of the queue to which the driver will send messages to
ConsumerQueue: the name of the queue to which the driver will receive messages from
InputTemplateFile: the template xml message file. This file should conform the wsdl in use
InputBatchSize: number of messages to send in one batch
OutputBatchSize: number of messages to receive after sending a batch
TotalBatches: number of batches to send
MessageDeliveryMode: possible values are PERSISTENT, NON_PERSISTENT. 

For example:

LoggingLevel=FINE
LogFile=benchmark.log
JmsServerHostName=localhost
JmsServerPort=7676
PublisherQueue=iep_echo_benchmark_publish
ConsumerQueue=iep_echo_benchmark_consume
InputTemplateFile=input.txt
InputBatchSize=1000
OutputBatchSize=1000
TotalBatches=5
MessageDeliveryMode=NON_PERSISTENT

2. How to run?
RunBenchmark <benchmark_property_file>

For example:
C:\open-jbi-components\driver-tests\performance\JMSBased>RunBenchmark.bat benchmarks\iepse\echo\benchmark.properties