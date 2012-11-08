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

import java.io.FileInputStream;
import java.io.InputStream;
public class TestPicture {


      public static void main(String[] args) throws Exception {
          InputStream is=new FileInputStream(args[0]);
          CommareaLexer lexer = new CommareaLexer(is);
          CommareaParser parser = new CommareaParser(lexer);
          parser.picture_definition();
          /*parser.picture_definition();
          parser.picture_definition();
          parser.picture_definition();
          parser.picture_definition();
          parser.picture_definition();
          parser.picture_definition();
          parser.picture_definition();*/
      }
  }


