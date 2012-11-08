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
import it.imolinfo.jbi4cics.typemapping.cobol.CobolTypeDescriptor;

import java.io.FileInputStream;
import java.io.InputStream;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class OccursCommareaParserTest extends BaseCommareaTest {

  private static Log log = LogFactory.getLog(OccursCommareaParserTest.class);


  public OccursCommareaParserTest(String arg) {
    super(arg);
  }
  
  public void testNestedCommarea() throws Exception {
    InputStream is = new FileInputStream(testRootCommarea+"/"+"OccursCommarea.txt");
    CommareaLexer lexer = new CommareaLexer(is);
    CommareaParser parser = new CommareaParser(lexer);
    CommareaBeanMappingDescriptor commareaBeanMappingDescriptor = parser.commarea_definition();
    log.info("commareaBeanMappingDescriptor con gestione del nesting: " + commareaBeanMappingDescriptor);
    CommareaBeanMappingDescriptor DFHCOMMAREA=((CobolTypeDescriptor)commareaBeanMappingDescriptor.getFieldMap().get("DFHCOMMAREA")).getNestedCommarea();
    log.debug("DFHCOMMAREA: "+DFHCOMMAREA);
    CobolTypeDescriptor CA_SWSECI1_COMMAREA_CobolType=((CobolTypeDescriptor)DFHCOMMAREA.getFieldMap().get("CA_SWSECI1_COMMAREA"));
    assertNotNull(CA_SWSECI1_COMMAREA_CobolType);
    log.debug("CA_SWSECI1_COMMAREA cobol type: "+CA_SWSECI1_COMMAREA_CobolType.getType());
    CommareaBeanMappingDescriptor CA_SWSECI1_COMMAREA=CA_SWSECI1_COMMAREA_CobolType.getNestedCommarea();
    CobolTypeDescriptor CA_ROW_DATA=((CobolTypeDescriptor)CA_SWSECI1_COMMAREA.getFieldMap().get("CA_ROW_DATA"));
    log.debug("CA_ROW_DATA cobol type: "+CA_ROW_DATA.getType());
    int occursSize=CA_ROW_DATA.getOccursSize();
    assertEquals("occursSize", 10, occursSize);
  }  
  
public void testNestedCommarea2() throws Exception {
    InputStream is = new FileInputStream(testRootCommarea+"/"+"OccursCommarea2.txt");
    CommareaLexer lexer = new CommareaLexer(is);
    CommareaParser parser = new CommareaParser(lexer);
    CommareaBeanMappingDescriptor commareaBeanMappingDescriptor = parser.commarea_definition();
    log.info("commareaBeanMappingDescriptor con gestione del nesting: " + commareaBeanMappingDescriptor);
  }  
}
