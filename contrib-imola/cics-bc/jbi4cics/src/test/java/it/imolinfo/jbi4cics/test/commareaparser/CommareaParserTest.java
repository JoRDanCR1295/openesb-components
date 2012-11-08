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
import it.imolinfo.jbi4cics.exception.ParseException;
import it.imolinfo.jbi4cics.messageformat.commarea.CommareaBeanMappingDescriptor;
import it.imolinfo.jbi4cics.test.BaseCommareaTest;
import it.imolinfo.jbi4cics.typemapping.cobol.CobolType;
import it.imolinfo.jbi4cics.typemapping.cobol.CobolTypeDescriptor;

import java.io.File;
import java.io.FileDescriptor;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.nio.charset.Charset;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class CommareaParserTest extends BaseCommareaTest {

  private static final Charset UTF8 = Charset.forName("UTF-8");

  private static Log log = LogFactory.getLog(CommareaParserTest.class);

  public CommareaParserTest(String arg) {
    super(arg);
  }

  public void testProvaCommarea() throws Exception {
    InputStream is = new FileInputStream(testRootCommarea+"/"+"ProvaCommarea.txt");
    CommareaLexer lexer = new CommareaLexer(is);
    CommareaParser parser = new CommareaParser(lexer);
    try {
      parser.commarea_definition();
      fail("il test dovrebbe dare un'eccezione");
    }
    catch (ParseException e){
      assertEquals("CIC000001_Occurs_not_supported", e.getMessage());
    }
  }

  public void testProvaCommarea2() throws Exception {
    Reader r = new FileReader(testRootCommarea + "/ProvaCommarea2.txt");

    verifyProvaCommarea2(r);
  }

  private static void verifyProvaCommarea2(Reader reader) throws Exception {

    // preparazione della commarea di confronto
    CommareaBeanMappingDescriptor expectedCommareaBeanMappingDescriptor=new CommareaBeanMappingDescriptor();
    CommareaBeanMappingDescriptor expectedCommareaBeanMappingDescriptor2=new CommareaBeanMappingDescriptor();
    CobolTypeDescriptor cobolTypeDescriptor;
    CobolTypeDescriptor cobolTypeDescriptor2;
//  02 CA-RETCODE1 PIC XXXXX        .
    cobolTypeDescriptor=new CobolTypeDescriptor();
    cobolTypeDescriptor2=new CobolTypeDescriptor();
    cobolTypeDescriptor.setType(CobolType.STRING);
    cobolTypeDescriptor2.setType(CobolType.STRING);
    cobolTypeDescriptor.setStringLength(5);
    cobolTypeDescriptor2.setStringLength(5);
    cobolTypeDescriptor.setName("CA-RETCODE1");
    cobolTypeDescriptor2.setName("CA-RETCODE1");
    cobolTypeDescriptor.setPadCharacter(" ");
    cobolTypeDescriptor.setJustification(CobolTypeDescriptor.STRING_JUSTIFICATION_LEFT);
    cobolTypeDescriptor2.setPadCharacter(" ");
    cobolTypeDescriptor2.setJustification(CobolTypeDescriptor.STRING_JUSTIFICATION_LEFT);
    cobolTypeDescriptor.setLevel(2);
    cobolTypeDescriptor2.setLevel(2);
    expectedCommareaBeanMappingDescriptor.addFieldMapping(cobolTypeDescriptor.getName().replace('-','_'),cobolTypeDescriptor.getName(),cobolTypeDescriptor);
    expectedCommareaBeanMappingDescriptor2.addFieldMapping(cobolTypeDescriptor2.getName().replace('-','_'),cobolTypeDescriptor2.getName(),cobolTypeDescriptor2);
    assertEquals(expectedCommareaBeanMappingDescriptor,expectedCommareaBeanMappingDescriptor2);
//  02 CA-RETCODE2 PIC AAAAA        .
    cobolTypeDescriptor=new CobolTypeDescriptor();
    cobolTypeDescriptor.setType(CobolType.STRING);
    cobolTypeDescriptor.setStringLength(5);
    cobolTypeDescriptor.setName("CA-RETCODE2");
    cobolTypeDescriptor.setPadCharacter(" ");
    cobolTypeDescriptor.setJustification(CobolTypeDescriptor.STRING_JUSTIFICATION_LEFT);
    cobolTypeDescriptor.setLevel(2);
    expectedCommareaBeanMappingDescriptor.addFieldMapping(cobolTypeDescriptor.getName().replace('-','_'),cobolTypeDescriptor.getName(),cobolTypeDescriptor);
//  02 CA-RETCODE3 PIC A(5)         .
    cobolTypeDescriptor=new CobolTypeDescriptor();
    cobolTypeDescriptor.setType(CobolType.STRING);
    cobolTypeDescriptor.setStringLength(5);
    cobolTypeDescriptor.setName("CA-RETCODE3");
    cobolTypeDescriptor.setPadCharacter(" ");
    cobolTypeDescriptor.setJustification(CobolTypeDescriptor.STRING_JUSTIFICATION_LEFT);
    cobolTypeDescriptor.setLevel(2);
    expectedCommareaBeanMappingDescriptor.addFieldMapping(cobolTypeDescriptor.getName().replace('-','_'),cobolTypeDescriptor.getName(),cobolTypeDescriptor);
//    02 CA-RETCODE4 PIC X(5)         .
    cobolTypeDescriptor=new CobolTypeDescriptor();
    cobolTypeDescriptor.setType(CobolType.STRING);
    cobolTypeDescriptor.setStringLength(5);
    cobolTypeDescriptor.setName("CA-RETCODE4");
    cobolTypeDescriptor.setPadCharacter(" ");
    cobolTypeDescriptor.setJustification(CobolTypeDescriptor.STRING_JUSTIFICATION_LEFT);
    cobolTypeDescriptor.setLevel(2);
    expectedCommareaBeanMappingDescriptor.addFieldMapping(cobolTypeDescriptor.getName().replace('-','_'),cobolTypeDescriptor.getName(),cobolTypeDescriptor);
//    02 CA-RETCODE5 PIC 99999        .
    cobolTypeDescriptor=new CobolTypeDescriptor();
    cobolTypeDescriptor.setType(CobolType.ZONED);
    cobolTypeDescriptor.setIntegerPartLength(5);
    cobolTypeDescriptor.setName("CA-RETCODE5");
    cobolTypeDescriptor.setLevel(2);
    expectedCommareaBeanMappingDescriptor.addFieldMapping(cobolTypeDescriptor.getName().replace('-','_'),cobolTypeDescriptor.getName(),cobolTypeDescriptor);
//    02 CA-RETCODE6 PIC 9(5)         .
    cobolTypeDescriptor=new CobolTypeDescriptor();
    cobolTypeDescriptor.setType(CobolType.ZONED);
    cobolTypeDescriptor.setIntegerPartLength(5);
    cobolTypeDescriptor.setName("CA-RETCODE6");
    cobolTypeDescriptor.setLevel(2);
    expectedCommareaBeanMappingDescriptor.addFieldMapping(cobolTypeDescriptor.getName().replace('-','_'),cobolTypeDescriptor.getName(),cobolTypeDescriptor);
//    02 CA-RETCODE7 PIC S99999       .
    cobolTypeDescriptor=new CobolTypeDescriptor();
    cobolTypeDescriptor.setType(CobolType.ZONED);
    cobolTypeDescriptor.setIntegerPartLength(5);
    cobolTypeDescriptor.setName("CA-RETCODE7");
    cobolTypeDescriptor.setSigned(true);
    cobolTypeDescriptor.setLevel(2);
    expectedCommareaBeanMappingDescriptor.addFieldMapping(cobolTypeDescriptor.getName().replace('-','_'),cobolTypeDescriptor.getName(),cobolTypeDescriptor);
//    02 CA-RETCODE8 PIC S9(5)        .
    cobolTypeDescriptor=new CobolTypeDescriptor();
    cobolTypeDescriptor.setType(CobolType.ZONED);
    cobolTypeDescriptor.setIntegerPartLength(5);
    cobolTypeDescriptor.setName("CA-RETCODE8");
    cobolTypeDescriptor.setSigned(true);
    cobolTypeDescriptor.setLevel(2);
    expectedCommareaBeanMappingDescriptor.addFieldMapping(cobolTypeDescriptor.getName().replace('-','_'),cobolTypeDescriptor.getName(),cobolTypeDescriptor);
//    02 CA-RETCODE9 PIC +99999       .
    cobolTypeDescriptor=new CobolTypeDescriptor();
    cobolTypeDescriptor.setType(CobolType.ZONED);
    cobolTypeDescriptor.setIntegerPartLength(5);
    cobolTypeDescriptor.setName("CA-RETCODE9");
    cobolTypeDescriptor.setZonedSignFormat(CobolTypeDescriptor.SIGN_FORMAT_LEADING_SEPARATE);
    cobolTypeDescriptor.setSigned(true);
    cobolTypeDescriptor.setLevel(2);
    expectedCommareaBeanMappingDescriptor.addFieldMapping(cobolTypeDescriptor.getName().replace('-','_'),cobolTypeDescriptor.getName(),cobolTypeDescriptor);
//    02 CA-RETCODE10 PIC +9(5)       .
    cobolTypeDescriptor=new CobolTypeDescriptor();
    cobolTypeDescriptor.setType(CobolType.ZONED);
    cobolTypeDescriptor.setIntegerPartLength(5);
    cobolTypeDescriptor.setName("CA-RETCODE10");
    cobolTypeDescriptor.setZonedSignFormat(CobolTypeDescriptor.SIGN_FORMAT_LEADING_SEPARATE);
    cobolTypeDescriptor.setSigned(true);
    cobolTypeDescriptor.setLevel(2);
    expectedCommareaBeanMappingDescriptor.addFieldMapping(cobolTypeDescriptor.getName().replace('-','_'),cobolTypeDescriptor.getName(),cobolTypeDescriptor);
//    02 CA-RETCODE11 PIC 99999V999   .
    cobolTypeDescriptor=new CobolTypeDescriptor();
    cobolTypeDescriptor.setType(CobolType.ZONED);
    cobolTypeDescriptor.setIntegerPartLength(5);
    cobolTypeDescriptor.setDecimalPartLength(3);
    cobolTypeDescriptor.setName("CA-RETCODE11");
    cobolTypeDescriptor.setLevel(2);
    expectedCommareaBeanMappingDescriptor.addFieldMapping(cobolTypeDescriptor.getName().replace('-','_'),cobolTypeDescriptor.getName(),cobolTypeDescriptor);
//    02 CA-RETCODE12 PIC 9(5)V999    .
    cobolTypeDescriptor=new CobolTypeDescriptor();
    cobolTypeDescriptor.setType(CobolType.ZONED);
    cobolTypeDescriptor.setIntegerPartLength(5);
    cobolTypeDescriptor.setDecimalPartLength(3);
    cobolTypeDescriptor.setName("CA-RETCODE12");
    cobolTypeDescriptor.setLevel(2);
    expectedCommareaBeanMappingDescriptor.addFieldMapping(cobolTypeDescriptor.getName().replace('-','_'),cobolTypeDescriptor.getName(),cobolTypeDescriptor);
//    02 CA-RETCODE13 PIC S99999V999  .
    cobolTypeDescriptor=new CobolTypeDescriptor();
    cobolTypeDescriptor.setType(CobolType.ZONED);
    cobolTypeDescriptor.setIntegerPartLength(5);
    cobolTypeDescriptor.setDecimalPartLength(3);
    cobolTypeDescriptor.setName("CA-RETCODE13");
    cobolTypeDescriptor.setSigned(true);
    cobolTypeDescriptor.setLevel(2);
    expectedCommareaBeanMappingDescriptor.addFieldMapping(cobolTypeDescriptor.getName().replace('-','_'),cobolTypeDescriptor.getName(),cobolTypeDescriptor);
//    02 CA-RETCODE14 PIC S9(5)V999   .
    cobolTypeDescriptor=new CobolTypeDescriptor();
    cobolTypeDescriptor.setType(CobolType.ZONED);
    cobolTypeDescriptor.setIntegerPartLength(5);
    cobolTypeDescriptor.setDecimalPartLength(3);
    cobolTypeDescriptor.setName("CA-RETCODE14");
    cobolTypeDescriptor.setSigned(true);
    cobolTypeDescriptor.setLevel(2);
    expectedCommareaBeanMappingDescriptor.addFieldMapping(cobolTypeDescriptor.getName().replace('-','_'),cobolTypeDescriptor.getName(),cobolTypeDescriptor);
//    02 CA-RETCODE15 PIC +99999V999  .
    cobolTypeDescriptor=new CobolTypeDescriptor();
    cobolTypeDescriptor.setType(CobolType.ZONED);
    cobolTypeDescriptor.setIntegerPartLength(5);
    cobolTypeDescriptor.setDecimalPartLength(3);
    cobolTypeDescriptor.setName("CA-RETCODE15");
    cobolTypeDescriptor.setZonedSignFormat(CobolTypeDescriptor.SIGN_FORMAT_LEADING_SEPARATE);
    cobolTypeDescriptor.setSigned(true);
    cobolTypeDescriptor.setLevel(2);
    expectedCommareaBeanMappingDescriptor.addFieldMapping(cobolTypeDescriptor.getName().replace('-','_'),cobolTypeDescriptor.getName(),cobolTypeDescriptor);
//    02 CA-RETCODE16 PIC +9(5)V999   .
    cobolTypeDescriptor=new CobolTypeDescriptor();
    cobolTypeDescriptor.setType(CobolType.ZONED);
    cobolTypeDescriptor.setIntegerPartLength(5);
    cobolTypeDescriptor.setDecimalPartLength(3);
    cobolTypeDescriptor.setName("CA-RETCODE16");
    cobolTypeDescriptor.setZonedSignFormat(CobolTypeDescriptor.SIGN_FORMAT_LEADING_SEPARATE);
    cobolTypeDescriptor.setSigned(true);
    cobolTypeDescriptor.setLevel(2);
    expectedCommareaBeanMappingDescriptor.addFieldMapping(cobolTypeDescriptor.getName().replace('-','_'),cobolTypeDescriptor.getName(),cobolTypeDescriptor);
//    02 CA-RETCODE17 PIC 99999V(3)   .
    cobolTypeDescriptor=new CobolTypeDescriptor();
    cobolTypeDescriptor.setType(CobolType.ZONED);
    cobolTypeDescriptor.setIntegerPartLength(5);
    cobolTypeDescriptor.setDecimalPartLength(3);
    cobolTypeDescriptor.setName("CA-RETCODE17");
    cobolTypeDescriptor.setLevel(2);
    expectedCommareaBeanMappingDescriptor.addFieldMapping(cobolTypeDescriptor.getName().replace('-','_'),cobolTypeDescriptor.getName(),cobolTypeDescriptor);
//    02 CA-RETCODE18 PIC 9(5)V(3)    .
    cobolTypeDescriptor=new CobolTypeDescriptor();
    cobolTypeDescriptor.setType(CobolType.ZONED);
    cobolTypeDescriptor.setIntegerPartLength(5);
    cobolTypeDescriptor.setDecimalPartLength(3);
    cobolTypeDescriptor.setName("CA-RETCODE18");
    cobolTypeDescriptor.setLevel(2);
    expectedCommareaBeanMappingDescriptor.addFieldMapping(cobolTypeDescriptor.getName().replace('-','_'),cobolTypeDescriptor.getName(),cobolTypeDescriptor);
//    02 CA-RETCODE19 PIC S99999V(3)  .
    cobolTypeDescriptor=new CobolTypeDescriptor();
    cobolTypeDescriptor.setType(CobolType.ZONED);
    cobolTypeDescriptor.setIntegerPartLength(5);
    cobolTypeDescriptor.setDecimalPartLength(3);
    cobolTypeDescriptor.setName("CA-RETCODE19");
    cobolTypeDescriptor.setSigned(true);
    cobolTypeDescriptor.setLevel(2);
    expectedCommareaBeanMappingDescriptor.addFieldMapping(cobolTypeDescriptor.getName().replace('-','_'),cobolTypeDescriptor.getName(),cobolTypeDescriptor);
//    02 CA-RETCODE20 PIC S9(5)V(3)   .
    cobolTypeDescriptor=new CobolTypeDescriptor();
    cobolTypeDescriptor.setType(CobolType.ZONED);
    cobolTypeDescriptor.setIntegerPartLength(5);
    cobolTypeDescriptor.setDecimalPartLength(3);
    cobolTypeDescriptor.setName("CA-RETCODE20");
    cobolTypeDescriptor.setSigned(true);
    cobolTypeDescriptor.setLevel(2);
    expectedCommareaBeanMappingDescriptor.addFieldMapping(cobolTypeDescriptor.getName().replace('-','_'),cobolTypeDescriptor.getName(),cobolTypeDescriptor);
//    02 CA-RETCODE21 PIC +99999V(3)  .
    cobolTypeDescriptor=new CobolTypeDescriptor();
    cobolTypeDescriptor.setType(CobolType.ZONED);
    cobolTypeDescriptor.setIntegerPartLength(5);
    cobolTypeDescriptor.setDecimalPartLength(3);
    cobolTypeDescriptor.setName("CA-RETCODE21");
    cobolTypeDescriptor.setZonedSignFormat(CobolTypeDescriptor.SIGN_FORMAT_LEADING_SEPARATE);
    cobolTypeDescriptor.setSigned(true);
    cobolTypeDescriptor.setLevel(2);
    expectedCommareaBeanMappingDescriptor.addFieldMapping(cobolTypeDescriptor.getName().replace('-','_'),cobolTypeDescriptor.getName(),cobolTypeDescriptor);
//    02 CA-RETCODE22 PIC +9(5)V(3)   .
    cobolTypeDescriptor=new CobolTypeDescriptor();
    cobolTypeDescriptor.setType(CobolType.ZONED);
    cobolTypeDescriptor.setIntegerPartLength(5);
    cobolTypeDescriptor.setDecimalPartLength(3);
    cobolTypeDescriptor.setName("CA-RETCODE22");
    cobolTypeDescriptor.setZonedSignFormat(CobolTypeDescriptor.SIGN_FORMAT_LEADING_SEPARATE);
    cobolTypeDescriptor.setSigned(true);
    cobolTypeDescriptor.setLevel(2);
    expectedCommareaBeanMappingDescriptor.addFieldMapping(cobolTypeDescriptor.getName().replace('-','_'),cobolTypeDescriptor.getName(),cobolTypeDescriptor);

    CommareaLexer lexer = new CommareaLexer(reader);
    CommareaParser parser = new CommareaParser(lexer);
    CommareaBeanMappingDescriptor commareaBeanMappingDescriptor = parser.commarea_definition();
    log.info("commareaBeanMappingDescriptor: " + commareaBeanMappingDescriptor);
    assertEquals(expectedCommareaBeanMappingDescriptor,commareaBeanMappingDescriptor);
  }

  public void testVinciCommarea2() throws Exception {
    InputStream is = new FileInputStream(testRootCommarea+"/"+"VinciCommarea2.txt");
    CommareaLexer lexer = new CommareaLexer(is);
    CommareaParser parser = new CommareaParser(lexer);
    CommareaBeanMappingDescriptor commareaBeanMappingDescriptor = parser.commarea_definition();
    log.info("commareaBeanMappingDescriptor: " + commareaBeanMappingDescriptor);
  }

  public void testVinciCommarea() throws Exception {
    InputStream is = new FileInputStream(testRootCommarea+"/"+"VinciCommarea.txt");
    CommareaLexer lexer = new CommareaLexer(is);
    CommareaParser parser = new CommareaParser(lexer);
    CommareaBeanMappingDescriptor commareaBeanMappingDescriptor = parser.commarea_definition();
    log.info("commareaBeanMappingDescriptor: " + commareaBeanMappingDescriptor);
  }

  public void testCommareaStartingWithSpace() throws Exception {
    doParse("CommareaStartingWithSpace.txt");
  }

  public void testCommareaStartingWithTab() throws Exception {
    doParse("CommareaStartingWithTab.txt");
  }

  public void testCommareaWithTrailingSpaces() throws Exception {
    doParse("CommareaWithTrailingSpaces.txt");
  }

  public void testCommareaStartingWithBlankLine() throws Exception {
    doParse("CommareaStartingWithBlankLine.txt");
  }

  public void testCommareaStartingWithBlankLines() throws Exception {
    doParse("CommareaStartingWithBlankLines.txt");
  }

  private static void doParse(final String fileName) throws Exception {
    InputStream is = new FileInputStream(testRootCommarea + "/" + fileName);
    CommareaLexer lexer = new CommareaLexer(is);
    CommareaParser parser = new CommareaParser(lexer);

    parser.commarea_definition();
  }

  public void testStarComment() throws Exception {

    // This commarea contains special characters, so is written on a UTF-8 file
    Reader r = new InputStreamReader(new FileInputStream(
            "src/test/etc/commareas/CommareaWithStarComment.txt"), UTF8);

    verifyProvaCommarea2(r);
  }

  public void testSlashComment() throws Exception {

    // This commarea contains special characters, so is written on a UTF-8 file
    Reader r = new InputStreamReader(new FileInputStream(
            "src/test/etc/commareas/CommareaWithSlashComment.txt"), UTF8);

    verifyProvaCommarea2(r);
  }

  public void testCommentAtBeginOfFile() throws Exception {
    Reader r = new StringReader("*Commento a inizio file \r\n"
                                + "02 CA-RETCODE1 PIC XXXXX        .\n"
                                + "02 CA-RETCODE2 PIC AAAAA        .\n"
                                + "02 CA-RETCODE3 PIC A(5)         .\n"
                                + "02 CA-RETCODE4 PIC X(5)         .\n"
                                + "02 CA-RETCODE5 PIC 99999        .\n"
                                + "02 CA-RETCODE6 PIC 9(5)         .\n"
                                + "02 CA-RETCODE7 PIC S99999       .\n"
                                + "02 CA-RETCODE8 PIC S9(5)        .\n"
                                + "02 CA-RETCODE9 PIC +99999       .\n"
                                + "02 CA-RETCODE10 PIC +9(5)       .\n"
                                + "02 CA-RETCODE11 PIC 99999V999   .\n"
                                + "02 CA-RETCODE12 PIC 9(5)V999    .\n"
                                + "02 CA-RETCODE13 PIC S99999V999  .\n"
                                + "02 CA-RETCODE14 PIC S9(5)V999   .\n"
                                + "02 CA-RETCODE15 PIC +99999V999  .\n"
                                + "02 CA-RETCODE16 PIC +9(5)V999   .\n"
                                + "02 CA-RETCODE17 PIC 99999V(3)   .\n"
                                + "02 CA-RETCODE18 PIC 9(5)V(3)    .\n"
                                + "02 CA-RETCODE19 PIC S99999V(3)  .\n"
                                + "02 CA-RETCODE20 PIC S9(5)V(3)   .\n"
                                + "02 CA-RETCODE21 PIC +99999V(3)  .\n"
                                + "02 CA-RETCODE22 PIC +9(5)V(3)   .\n"
                                + "\n");

    verifyProvaCommarea2(r);
  }

  public void testCommentAtBeginOfFileWithBlankLines() throws Exception {
    Reader r = new StringReader("                   \n"
                                + "*Commento a inizio file \r\n"
                                + "\n"
                                + "02 CA-RETCODE1 PIC XXXXX        .\n"
                                + "02 CA-RETCODE2 PIC AAAAA        .\n"
                                + "02 CA-RETCODE3 PIC A(5)         .\n"
                                + "02 CA-RETCODE4 PIC X(5)         .\n"
                                + "02 CA-RETCODE5 PIC 99999        .\n"
                                + "02 CA-RETCODE6 PIC 9(5)         .\n"
                                + "02 CA-RETCODE7 PIC S99999       .\n"
                                + "02 CA-RETCODE8 PIC S9(5)        .\n"
                                + "02 CA-RETCODE9 PIC +99999       .\n"
                                + "02 CA-RETCODE10 PIC +9(5)       .\n"
                                + "02 CA-RETCODE11 PIC 99999V999   .\n"
                                + "02 CA-RETCODE12 PIC 9(5)V999    .\n"
                                + "02 CA-RETCODE13 PIC S99999V999  .\n"
                                + "02 CA-RETCODE14 PIC S9(5)V999   .\n"
                                + "02 CA-RETCODE15 PIC +99999V999  .\n"
                                + "02 CA-RETCODE16 PIC +9(5)V999   .\n"
                                + "02 CA-RETCODE17 PIC 99999V(3)   .\n"
                                + "02 CA-RETCODE18 PIC 9(5)V(3)    .\n"
                                + "02 CA-RETCODE19 PIC S99999V(3)  .\n"
                                + "02 CA-RETCODE20 PIC S9(5)V(3)   .\n"
                                + "02 CA-RETCODE21 PIC +99999V(3)  .\n"
                                + "02 CA-RETCODE22 PIC +9(5)V(3)   .\n"
                                + "\n");

    verifyProvaCommarea2(r);
  }

  public void testCommentAtEndOfFile() throws Exception {
    Reader r = new StringReader("02 CA-RETCODE1 PIC XXXXX        .\n"
                                + "02 CA-RETCODE2 PIC AAAAA        .\n"
                                + "02 CA-RETCODE3 PIC A(5)         .\n"
                                + "02 CA-RETCODE4 PIC X(5)         .\n"
                                + "02 CA-RETCODE5 PIC 99999        .\n"
                                + "02 CA-RETCODE6 PIC 9(5)         .\n"
                                + "02 CA-RETCODE7 PIC S99999       .\n"
                                + "02 CA-RETCODE8 PIC S9(5)        .\n"
                                + "02 CA-RETCODE9 PIC +99999       .\n"
                                + "02 CA-RETCODE10 PIC +9(5)       .\n"
                                + "02 CA-RETCODE11 PIC 99999V999   .\n"
                                + "02 CA-RETCODE12 PIC 9(5)V999    .\n"
                                + "02 CA-RETCODE13 PIC S99999V999  .\n"
                                + "02 CA-RETCODE14 PIC S9(5)V999   .\n"
                                + "02 CA-RETCODE15 PIC +99999V999  .\n"
                                + "02 CA-RETCODE16 PIC +9(5)V999   .\n"
                                + "02 CA-RETCODE17 PIC 99999V(3)   .\n"
                                + "02 CA-RETCODE18 PIC 9(5)V(3)    .\n"
                                + "02 CA-RETCODE19 PIC S99999V(3)  .\n"
                                + "02 CA-RETCODE20 PIC S9(5)V(3)   .\n"
                                + "02 CA-RETCODE21 PIC +99999V(3)  .\n"
                                + "02 CA-RETCODE22 PIC +9(5)V(3)   .\n"
                                + "* Ciao \n");

    verifyProvaCommarea2(r);
  }

  public void testCodeCommented() throws Exception {
    Reader r = new StringReader("02 CA-RETCODE1 PIC XXXXX          .\n"
                                + "/02 CA-RETCODE2 PIC AAA          .\n"
                                + "02 CA-RETCODE2 PIC AAAAA        .\n"
                                + "02 CA-RETCODE3 PIC A(5)         .\n"
                                + "02 CA-RETCODE4 PIC X(5)         .\n"
                                + "02 CA-RETCODE5 PIC 99999        .\n"
                                + "02 CA-RETCODE6 PIC 9(5)         .\n"
                                + "02 CA-RETCODE7 PIC S99999       .\n"
                                + "02 CA-RETCODE8 PIC S9(5)        .\n"
                                + "02 CA-RETCODE9 PIC +99999       .\n"
                                + "02 CA-RETCODE10 PIC +9(5)       .\n"
                                + "02 CA-RETCODE11 PIC 99999V999   .\n"
                                + "02 CA-RETCODE12 PIC 9(5)V999    .\n"
                                + "02 CA-RETCODE13 PIC S99999V999  .\n"
                                + "02 CA-RETCODE14 PIC S9(5)V999   .\n"
                                + "02 CA-RETCODE15 PIC +99999V999  .\n"
                                + "02 CA-RETCODE16 PIC +9(5)V999   .\n"
                                + "02 CA-RETCODE17 PIC 99999V(3)   .\n"
                                + "02 CA-RETCODE18 PIC 9(5)V(3)    .\n"
                                + "02 CA-RETCODE19 PIC S99999V(3)  .\n"
                                + "02 CA-RETCODE20 PIC S9(5)V(3)   .\n"
                                + "02 CA-RETCODE21 PIC +99999V(3)  .\n"
                                + "02 CA-RETCODE22 PIC +9(5)V(3)   .\n"
                                + "*02 CA-RETCODE23 PIC +9(5)V(3)   .\n");

    verifyProvaCommarea2(r);
  }

  public void testBlankLineInsideCode() throws Exception {
    Reader r = new StringReader("02 CA-RETCODE1 PIC XXXXX          .\n"
                                + "02 CA-RETCODE2 PIC AAAAA        .\n"
                                + "02 CA-RETCODE3 PIC A(5)         .\n"
                                + "02 CA-RETCODE4 PIC X(5)         .\n"
                                + "02 CA-RETCODE5 PIC 99999        .\n"
                                + "02 CA-RETCODE6 PIC 9(5)         .\n"
                                + "02 CA-RETCODE7 PIC S99999       .\n"
                                + "02 CA-RETCODE8 PIC S9(5)        .\n"
                                + "02 CA-RETCODE9 PIC +99999       .\n"
                                + "02 CA-RETCODE10 PIC +9(5)       .\n"
                                + "02 CA-RETCODE11 PIC 99999V999   .\n"
                                + "02 CA-RETCODE12 PIC 9(5)V999    .\n"
                                + "              \n"
                                + "02 CA-RETCODE13 PIC S99999V999  .\n"
                                + "02 CA-RETCODE14 PIC S9(5)V999   .\n"
                                + "02 CA-RETCODE15 PIC +99999V999  .\n"
                                + "02 CA-RETCODE16 PIC +9(5)V999   .\n"
                                + "02 CA-RETCODE17 PIC 99999V(3)   .\n"
                                + "02 CA-RETCODE18 PIC 9(5)V(3)    .\n"
                                + "02 CA-RETCODE19 PIC S99999V(3)  .\n"
                                + "02 CA-RETCODE20 PIC S9(5)V(3)   .\n"
                                + "02 CA-RETCODE21 PIC +99999V(3)  .\n"
                                + "02 CA-RETCODE22 PIC +9(5)V(3)   .\n"
                                + "\n");

    verifyProvaCommarea2(r);
  }

  public void testBlankLinesInsideCode() throws Exception {
    Reader r = new StringReader("02 CA-RETCODE1 PIC XXXXX          .\n"
                                + "02 CA-RETCODE2 PIC AAAAA        .\n"
                                + "02 CA-RETCODE3 PIC A(5)         .\n"
                                + "02 CA-RETCODE4 PIC X(5)         .\n"
                                + "02 CA-RETCODE5 PIC 99999        .\n"
                                + "02 CA-RETCODE6 PIC 9(5)         .\n"
                                + "02 CA-RETCODE7 PIC S99999       .\n"
                                + "02 CA-RETCODE8 PIC S9(5)        .\n"
                                + "02 CA-RETCODE9 PIC +99999       .\n"
                                + "02 CA-RETCODE10 PIC +9(5)       .\n"
                                + "02 CA-RETCODE11 PIC 99999V999   .\n"
                                + "02 CA-RETCODE12 PIC 9(5)V999    .\n"
                                + "              \n"
                                + " \t \n"
                                + "\t\n"
                                + "\n"
                                + "02 CA-RETCODE13 PIC S99999V999  .\n"
                                + "02 CA-RETCODE14 PIC S9(5)V999   .\n"
                                + "02 CA-RETCODE15 PIC +99999V999  .\n"
                                + "02 CA-RETCODE16 PIC +9(5)V999   .\n"
                                + "02 CA-RETCODE17 PIC 99999V(3)   .\n"
                                + "02 CA-RETCODE18 PIC 9(5)V(3)    .\n"
                                + "02 CA-RETCODE19 PIC S99999V(3)  .\n"
                                + "02 CA-RETCODE20 PIC S9(5)V(3)   .\n"
                                + "02 CA-RETCODE21 PIC +99999V(3)  .\n"
                                + "02 CA-RETCODE22 PIC +9(5)V(3)   .\n"
                                + "\n");

    verifyProvaCommarea2(r);
  }

  public void testCommareaEndingWithBlankLines() throws Exception {
    Reader r = new StringReader("02 CA-RETCODE1 PIC XXXXX          .\n"
                                + "02 CA-RETCODE2 PIC AAAAA        .\n"
                                + "02 CA-RETCODE3 PIC A(5)         .\n"
                                + "02 CA-RETCODE4 PIC X(5)         .\n"
                                + "02 CA-RETCODE5 PIC 99999        .\n"
                                + "02 CA-RETCODE6 PIC 9(5)         .\n"
                                + "02 CA-RETCODE7 PIC S99999       .\n"
                                + "02 CA-RETCODE8 PIC S9(5)        .\n"
                                + "02 CA-RETCODE9 PIC +99999       .\n"
                                + "02 CA-RETCODE10 PIC +9(5)       .\n"
                                + "02 CA-RETCODE11 PIC 99999V999   .\n"
                                + "02 CA-RETCODE12 PIC 9(5)V999    .\n"
                                + "02 CA-RETCODE13 PIC S99999V999  .\n"
                                + "02 CA-RETCODE14 PIC S9(5)V999   .\n"
                                + "02 CA-RETCODE15 PIC +99999V999  .\n"
                                + "02 CA-RETCODE16 PIC +9(5)V999   .\n"
                                + "02 CA-RETCODE17 PIC 99999V(3)   .\n"
                                + "02 CA-RETCODE18 PIC 9(5)V(3)    .\n"
                                + "02 CA-RETCODE19 PIC S99999V(3)  .\n"
                                + "02 CA-RETCODE20 PIC S9(5)V(3)   .\n"
                                + "02 CA-RETCODE21 PIC +99999V(3)  .\n"
                                + "02 CA-RETCODE22 PIC +9(5)V(3)   .\n"
                                + "              \n"
                                + "\t\n"
                                + "\n"
                                + "\r\n"
                                + " \t \n"
                                + "\t              \n");

    verifyProvaCommarea2(r);
  }

  public void testNoNewLineAtEndOfFile() throws Exception {
    Reader r = new StringReader("02 CA-RETCODE1 PIC XXXXX          .\n"
                                + "02 CA-RETCODE2 PIC AAAAA        .\n"
                                + "02 CA-RETCODE3 PIC A(5)         .\n"
                                + "02 CA-RETCODE4 PIC X(5)         .\n"
                                + "02 CA-RETCODE5 PIC 99999        .\n"
                                + "02 CA-RETCODE6 PIC 9(5)         .\n"
                                + "02 CA-RETCODE7 PIC S99999       .\n"
                                + "02 CA-RETCODE8 PIC S9(5)        .\n"
                                + "02 CA-RETCODE9 PIC +99999       .\n"
                                + "02 CA-RETCODE10 PIC +9(5)       .\n"
                                + "02 CA-RETCODE11 PIC 99999V999   .\n"
                                + "02 CA-RETCODE12 PIC 9(5)V999    .\n"
                                + "02 CA-RETCODE13 PIC S99999V999  .\n"
                                + "02 CA-RETCODE14 PIC S9(5)V999   .\n"
                                + "02 CA-RETCODE15 PIC +99999V999  .\n"
                                + "02 CA-RETCODE16 PIC +9(5)V999   .\n"
                                + "02 CA-RETCODE17 PIC 99999V(3)   .\n"
                                + "02 CA-RETCODE18 PIC 9(5)V(3)    .\n"
                                + "02 CA-RETCODE19 PIC S99999V(3)  .\n"
                                + "02 CA-RETCODE20 PIC S9(5)V(3)   .\n"
                                + "02 CA-RETCODE21 PIC +99999V(3)  .\n"
                                + "02 CA-RETCODE22 PIC +9(5)V(3)   .");

    verifyProvaCommarea2(r);
  }

  public void testCommareaWithoutNewLines() throws Exception {
    CommareaLexer lexer
            = new CommareaLexer(new StringReader("02 CA-RETCODE1 PIC XXXXX."));
    CommareaParser parser = new CommareaParser(lexer);
    CommareaBeanMappingDescriptor actual = parser.commarea_definition();
    CommareaBeanMappingDescriptor expected
            = new CommareaBeanMappingDescriptor();
    CobolTypeDescriptor descriptor = new CobolTypeDescriptor();

    descriptor.setType(CobolType.STRING);
    descriptor.setStringLength(5);
    descriptor.setName("CA-RETCODE1");
    descriptor.setPadCharacter(" ");
    descriptor.setJustification(CobolTypeDescriptor.STRING_JUSTIFICATION_LEFT);
    descriptor.setLevel(2);
    expected.addFieldMapping(descriptor.getName().replace('-', '_'),
                             descriptor.getName(), descriptor);
    assertEquals("Commarea con valori inaspettati", expected, actual);
  }
}
