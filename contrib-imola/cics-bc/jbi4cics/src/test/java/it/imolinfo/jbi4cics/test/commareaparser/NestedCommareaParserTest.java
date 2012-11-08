/*******************************************************************************
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4cics.test.commareaparser;

import it.imolinfo.jbi4cics.commareaparser.CommareaLexer;
import it.imolinfo.jbi4cics.commareaparser.CommareaParser;
import it.imolinfo.jbi4cics.messageformat.commarea.CommareaBeanMappingDescriptor;
import it.imolinfo.jbi4cics.test.BaseCommareaTest;

import java.io.FileInputStream;
import java.io.InputStream;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class NestedCommareaParserTest extends BaseCommareaTest {

  private static Log log = LogFactory.getLog(NestedCommareaParserTest.class);

  public NestedCommareaParserTest(String arg) {
    super(arg);
  }
  
  public void testNestedCommarea() throws Exception {
    InputStream is = new FileInputStream(testRootCommarea+"/"+"NestedCommarea.txt");
    CommareaLexer lexer = new CommareaLexer(is);
    CommareaParser parser = new CommareaParser(lexer);
    CommareaBeanMappingDescriptor commareaBeanMappingDescriptor = parser.commarea_definition();
    log.info("commareaBeanMappingDescriptor con gestione del nesting: " + commareaBeanMappingDescriptor);
  }  
  
  public void testNestedCommarea2() throws Exception {
    InputStream is = new FileInputStream(testRootCommarea+"/"+"NestedCommarea2.txt");
    CommareaLexer lexer = new CommareaLexer(is);
    CommareaParser parser = new CommareaParser(lexer);
    CommareaBeanMappingDescriptor commareaBeanMappingDescriptor = parser.commarea_definition();
    log.info("commareaBeanMappingDescriptor con gestione del nesting: " + commareaBeanMappingDescriptor);
  }  
  
  public void testNestedCommarea3() throws Exception {
    InputStream is = new FileInputStream(testRootCommarea+"/"+"democommarea.txt");
    CommareaLexer lexer = new CommareaLexer(is);
    CommareaParser parser = new CommareaParser(lexer);
    CommareaBeanMappingDescriptor commareaBeanMappingDescriptor = parser.commarea_definition();
    log.info("commareaBeanMappingDescriptor con gestione del nesting: " + commareaBeanMappingDescriptor);
  }    
}
