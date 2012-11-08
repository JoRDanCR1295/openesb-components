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
 * Created on Jul 16, 2008,12:15:39 PM 
 *
 */
package org.camelse.test.file2file;

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
        
        // TODO create Camel routes here.

        // here is a sample which processes the input files
        // (leaving them in place - see the 'noop' flag)
        // then performs content based routing on the message
        // using XPath
        
        // file url prefix for the camel file endpoint.
        String projectDir = "/C:/home/chikkala/dev/ojc/main/open-jbi-components/contrib-other/camelse/examples/file2file/File2FileCamelJBIModule";
        
        String inFileURI = "file:"+ projectDir+ "/test/data?noop=true";
        String ukFileURI = "file:"+ projectDir+ "/build/messages/uk";
        String othersFileURI = "file:"+ projectDir+ "/build/messages/others";
                
        from(inFileURI).
            choice().
                when(xpath("//person/city = 'London'")).to(ukFileURI).
                otherwise().to(othersFileURI);
        
        // Use this route when receiving messages from jbi endpoint
        // jbi uri format = "jbi:<service-namesapce>/<service-name>/<endpoint-name>
        
        // String jbiURI = "jbi:http://openesb.org/jbi2camel/File2FileCamelJBIModule/File2FileCamelJBIModule_service/jbi2camel_endpoint";
        // from(jbiURI).
        //     choice().
        //         when(xpath("//person/city = 'London'")).to(ukFileURI).
        //         otherwise().to(othersFileURI);
        //
    }
}