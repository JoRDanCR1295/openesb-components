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
 * Created on Jul 15, 2008,12:35:15 PM 
 *
 */
package org.camelse.test.file2jbi;

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

        // This router configuration demonstrates how to invoke services in 
        // other jbi engines (e.g. bpel) from camel and vice versa.
        // file(camel) -> jbi(bpel) -> jbi(camel) -> xpath -> file(camel)
        
        String projectDir = "/C:/home/chikkala/dev/ojc/main/open-jbi-components/contrib-other/camelse/examples/file2jbi/File2JBICamelJBIModule";
        
        String inFileURI = "file:"+ projectDir+ "/test/data?noop=true&consumer.recursive=false";
        String ukFileURI = "file:"+ projectDir+ "/build/messages/uk";
        String othersFileURI = "file:"+ projectDir+ "/build/messages/others";
        
        // route to receive messsages from file and send it to bpel process
        String toJbiURI = "jbi:http://openesb.org/camel2jbi/File2JBICamelJBIModule/Camel2BpelJBIModule_service/camel2jbi_endpoint";      
        from(inFileURI).
            to(toJbiURI);
        
        // This route receives message from bpel process and sends it to file
         String fromJbiURI = "jbi:http://openesb.org/jbi2camel/File2JBICamelJBIModule/File2JBICamelJBIModule_service/jbi2camel_endpoint";
         from(fromJbiURI).
             choice().
                 when(xpath("//person/city = 'London'")).to(ukFileURI).
                 otherwise().to(othersFileURI);
        
    }
}