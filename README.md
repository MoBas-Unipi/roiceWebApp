# roiceWebApp - Distributed Web Application
## Architecture

The application has been designed as a distributed system and has been deployed as shown in the following architecture diagram:

<p align="center">
<img src="doc/system_architecture.png" alt="System Architecture" width="600" height="auto">
</p>

## Main Components

- **Apache Tomcat Server (Spring Boot)**
- **MongoDB Database**
- **Nginx Load Balancer**
- **Erlang Master Node**
- **Erlang Server Nodes**



## Execution Options

You can either run the application **locally** on a single machine or in a **distributed** setting as depicted in the architecture diagram. To use it in a distributed way, the installation of Nginx is required.



### Nginx Installation and Configuration

To install **Nginx**, follow the [Nginx installation guide](https://docs.nginx.com/nginx/admin-guide/installing-nginx/installing-nginx-open-source/).

The **Nginx** configuration used is the following one (it has to be modified in /etc/nginx/nginx.conf):

```
worker_processes 1;
worker_rlimit_nofile 10000;

events {
    worker_connections 1024;
    # multi_accept on;
}

http {
    map $http_upgrade $connection_upgrade {
        default upgrade;
        '' close;
    }
    
    upstream websocket {
        ip_hash;
        server 10.2.1.42:8300;
        server 10.2.1.43:8300;
        server 10.2.1.44:8300;
    }
    
    server {
        listen 8300;
        location / {
            proxy_pass http://websocket;
            proxy_http_version 1.1;
            proxy_set_header Upgrade $http_upgrade;
            proxy_set_header Connection $connection_upgrade;
            proxy_set_header Host $host;
        }
    }
}

```



## Commands to Run the Application

In order to execute the application, the following commands can be used, make sure to replace the IP addresses with the ones needed in your case. 



### Execute the Java Spring Application (Tomcat Server)

To generate the `.war` file, use Maven:

```
mvn clean package
```

To copy the generated WAR file to a remote node (in case of distributed execution):

```
scp roiceWebApp-0.0.1-SNAPSHOT.war root@10.2.1.39:/root/
```

**Basic execution (using parameters defined in application.properties):**

```
java -jar roiceWebApp-0.0.1-SNAPSHOT.war
```

**Execution with additional parameters:**

```
java -jar roiceWebApp-0.0.1-SNAPSHOT.war --spring.data.mongodb.uri=mongodb://10.2.1.40:27017/ --server.address=10.2.1.39
```



### Execution of Erlang Applications

#### Local

To execute the Erlang applications locally, use the following commands inside their respective folders:

**Inside the `erws` folder:**

```sh
rebar3 shell --name server@127.0.0.1 --setcookie roice
```

**Inside the `master` folder:**

```
rebar3 shell --name master@127.0.0.1 --setcookie roice
```

#### Remote

**Copying Files to Remote Nodes**

To copy the necessary files to remote nodes, use the `scp` command, for example:

**Copy files from `erws` folder:**

```
scp -r erws/ root@10.2.1.44:/root/
```

**Copy files from `master` folder:**

```
scp -r master/ root@10.2.1.41:/root/
```

#### Execution on Containers

To execute the Erlang applications in a distributed way (on different containers):

**Inside the `erws` folder:**

```
rebar3 shell --name root@10.2.1.42 --setcookie roice
rebar3 shell --name root@10.2.1.43 --setcookie roice
rebar3 shell --name root@10.2.1.44 --setcookie roice
```

**Inside the `master` folder:**

```
rebar3 shell --name master@10.2.1.41 --setcookie roice
```



### MongoDB Commands

To set up the MongoDB collections, use the following commands:

**In `mongosh`:**

```
use webApp
db.createCollection("phones")
db.createCollection("users")
```

**From the terminal:**

```
mongoimport --host 127.0.0.1 --port 27017 --db webApp --collection phones --jsonArray --file webApp.phones.json
mongoimport --host 127.0.0.1 --port 27017 --db webApp --collection users --jsonArray --file webApp.users.json
```
