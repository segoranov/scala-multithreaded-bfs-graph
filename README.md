## University Project, run BFS algorithm on graph, starting from all vertices, with specified number of tasks (threads) using Scala

### After creating an executable jar (for example with sbt-assembly), run the program as explained below.

    Usage: java -jar <file.jar> -i <graph-file.in> -t <number of tasks> [-o <graph-data.out>] [-q]
           java -jar <file.jar> -n <number_of_vertices> -t <number of tasks> [-o <graph-data.out>] [-q]

    Mandatory parameters:
      -i -> input file with adjacency matrix to read graph from
      -n -> number of vertices in order to create graph with random generated edges
      -t -> number of tasks for the BFS algorithm

    The parameters -i and -n are mutually exclusive. Only one of the two should be present.

    Optional parameters:
      -o - output file for results. If not present, no results will be written to an output file.
      -q (quiet) - if present, only the total time taken for the BFS will be written to an output file with name 'graph-bfs-quiet-result.txt'.

    The program will always log information in file named 'graph-bfs.log' such as:

    10:25:37.124 [pool-1-thread-5] DEBUG graph.Graph - Start BFS from vertex 10
    10:25:37.339 [pool-1-thread-5] DEBUG graph.Graph - Finish BFS started from vertex 10. Time elapsed in milliseconds: 210

    and also log errors such as:

    10:40:17.914 [main] ERROR graph.GraphApp$ - Not all mandatory parameters are present!
