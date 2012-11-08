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
 * Created on Aug 4, 2008,10:04:07 AM 
 *
 */
package org.camelse.test.jbi2jbi;

import org.apache.camel.Exchange;
import org.apache.camel.Processor;
import org.apache.camel.builder.RouteBuilder;
import org.apache.camel.spring.Main;

import static org.apache.camel.builder.xml.XPathBuilder.xpath;
/**
 * A Camel Router that recevies messages from jbi endpoint and send messages
 * to another jbi endpiont.
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

        // This sample shows how to receive messages from jbi endpoints and 
        // send messages to jbi endpoints.
        
        // This route receives message from a jbi endpoint and then performs
        // content based routing on the message usng XPath to send the message
        // to either jbi endpoint or a pojo endpoint. 
        
        // jbi uri format = "jbi:<service_namesapce>/<service_name>/<endpoint_name>

         String inJbiURI = "jbi:http://openesb.org/jbi2camel/JBI2JBICamelJBIModule/JBI2JBICamelJBIModule_service/jbi2camel_endpoint";
         String outJbiURI = "jbi:http://openesb.org/wsdl/camel2jbi/JBI2JBICamelJBIModule/JBI2JBICamelJBIModule_service/camel2jbi_endpoint";
         
         from(inJbiURI).
             choice().
                 when(xpath("//person/city = 'London'")).to(outJbiURI).
                 otherwise().to("bean:otherPojo");
        
    }
}