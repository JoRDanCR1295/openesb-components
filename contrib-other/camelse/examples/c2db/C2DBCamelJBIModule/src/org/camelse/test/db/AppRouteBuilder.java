/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
/*
 * AppRouteBuilder.java
 *
 * Created on Nov 16, 2008,11:48:26 AM 
 *
 */
package org.camelse.test.db;

import org.apache.camel.Exchange;
import org.apache.camel.Processor;
import org.apache.camel.builder.RouteBuilder;
import org.apache.camel.spring.Main;

import static org.apache.camel.builder.xml.XPathBuilder.xpath;

/**
 * A Camel Router
 * @author chikkala
 */
public class AppRouteBuilder extends RouteBuilder {

    /**
     * A main() so we can easily run these routing rules in our IDE
     */
    public static void main(String... args) {
        Main.main(args);
    }

    /**
     * Lets configure the Camel routing rules using Java code...
     */
    public void configure() {
        // uses a POJO to print messages passing between two jbi endpoints.
        // jbi uri format = "jbi:<service-namesapce>/<service-name>/<endpoint-name>       
        String jbiInURI = "jbi:http://openesb.org/jbi2camel/DBCamelJBIModule/DBCamelJBIModule_service1/jbi2camel_endpoint";
        String jbiOutURI1 = "jbi:http://openesb.org/jbi2camel/DBCamelJBIModule/DBCamelJBIModule_service2/camel2jbi_endpoint";
        
        from(jbiInURI).to(jbiOutURI1); //TODO: add any other message processing in between the endpoints.
        // use this to log input requests using wire tap eip.
        // from(jbiInURI).multicast().to("bean:logPojo", jbiOutURI1);
    }
}