 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.test.jbi;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;
import junit.framework.TestCase;

import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.methods.InputStreamRequestEntity;
import org.apache.commons.httpclient.methods.PostMethod;


public class JBIServiceTest extends TestCase {

  private static Logger log = LoggerFactory.getLogger(JBIServiceTest.class);

  public JBIServiceTest(String arg0) {
    super(arg0);
  }

  public void testVinciServiceViaSOAPHTTP() {
    try {
      HttpClient httpClient = new HttpClient();
      PostMethod postMethod = new PostMethod("http://localhost:8192/Service/EchoService/");
      postMethod.setRequestEntity(new InputStreamRequestEntity(this.getClass().getClassLoader().getResourceAsStream("xmlmessages/testEchoMessage.xml")));
      // postMethod.setFollowRedirects(true);
      httpClient.executeMethod(postMethod);
      // postMethod.get
      String response = postMethod.getResponseBodyAsString();
      log.info("response: " + response);
    } catch (Exception e) {
      e.printStackTrace();
      log.error("errore durenate il test: " + e);
      fail(e.getMessage());
    }
  }

}
