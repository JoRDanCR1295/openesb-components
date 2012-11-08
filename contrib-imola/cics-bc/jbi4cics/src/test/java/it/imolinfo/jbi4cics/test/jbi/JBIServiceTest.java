/*******************************************************************************
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4cics.test.jbi;

import java.io.FileInputStream;

import junit.framework.TestCase;

import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.methods.InputStreamRequestEntity;
import org.apache.commons.httpclient.methods.PostMethod;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class JBIServiceTest extends TestCase {
  
  private static Log log = LogFactory.getLog(JBIServiceTest.class);
  public static final String testRootJbi=System.getProperty("jbi4cics.test.configurationRoot")+"/../resources/xmlmessages";
  
  public JBIServiceTest(String arg0) {
    super(arg0);
  }  
  
  /*public void testVinciService() {
    try{
      ServiceMixClient client=new RemoteServiceMixClient();
      InOut exchange = client.createInOutExchange();
  
      NormalizedMessage inMessage = exchange.getInMessage();
      //inMessage.setProperty("name", "James");
      inMessage.setContent(new StreamSource(new FileReader(testRootJbi+"/testVinciMessage.xml")));
  
      //optionally specify the endpoint
      QName service = new QName("http://vinci.test.jbi4cics.imolinfo.it/VinciService", "VinciServiceJBIPort");
      exchange.setService(service);
  
      client.sendSync(exchange);
      NormalizedMessage outMessage = exchange.getOutMessage();
      Source source=outMessage.getContent();
      Transformer transformer=TransformerFactory.newInstance().newTransformer();
      StringWriter sw=new StringWriter();
      Result result=new StreamResult(sw);
      transformer.transform(source,result);
      log.debug("risultato: "+sw.toString());
    }
    catch (Exception e){
      log.error("errore durenate il test: "+e);
      fail(e.getMessage());
    }
  }*/
  
  /*public void testVinciServiceViaHTTP() {
    try{
      URLConnection connection = new URL("http://localhost:8912").openConnection();
      connection.setDoOutput(true);
      OutputStream os = connection.getOutputStream();
  
      // Post the request file.
      FileInputStream fis = new FileInputStream(testRootJbi+"/testVinciMessage7.xml");
      
      //Buffer
      byte[] buf = new byte[256];
      for (int c = fis.read(buf); c != -1; c = fis.read(buf)) {
        os.write(buf,0,c);
      }
      os.close();
      fis.close();
      log.info("message sent");
      // Read the response.
      BufferedReader in = new BufferedReader(new InputStreamReader(connection.getInputStream()));
      String inputLine;
      while ((inputLine = in.readLine()) != null) {
          log.info(inputLine);
      }
      in.close();
    }
    catch (Exception e){
      log.error("errore durenate il test: "+e);
      fail(e.getMessage());
    }
  }*/
  
  /*public void testVinciServiceViaSOAPHTTP() {
    try{
      URLConnection urlConnection = new URL("http://localhost:8192/Service/VinciService").openConnection();
      HttpURLConnection connection=(HttpURLConnection)urlConnection;
      //connection.setRequestMethod("POST");
      connection.setDoOutput(true);
      //connection.connect();
      OutputStream os = connection.getOutputStream();
  
      // Post the request file.
      FileInputStream fis = new FileInputStream(testRootJbi+"/testVinciMessage5.xml");
      
      //Buffer
      byte[] buf = new byte[256];
      for (int c = fis.read(buf); c != -1; c = fis.read(buf)) {
        os.write(buf,0,c);
      }
      os.close();
      fis.close();
      log.info("message sent");
      // Read the response.
      BufferedReader in = new BufferedReader(new InputStreamReader(connection.getInputStream()));
      String inputLine;
      while ((inputLine = in.readLine()) != null) {
          log.info(inputLine);
      }
      in.close();
      log.info("finished");
    }
    catch (Exception e){
      e.printStackTrace();
      log.error("errore durenate il test: "+e);
      fail(e.getMessage());
    }
  }*/   
  
  public void testVinciServiceViaSOAPHTTP2() {
    try{
      HttpClient httpClient=new HttpClient();
      PostMethod postMethod=new PostMethod("http://localhost:8192/Service/VinciService/");
      postMethod.setRequestEntity(new InputStreamRequestEntity(new FileInputStream(testRootJbi+"/testVinciMessage5.xml")));
      //postMethod.setFollowRedirects(true);
      httpClient.executeMethod(postMethod);
      //postMethod.get
      String response=postMethod.getResponseBodyAsString();
      log.info("response: "+response);
      
      /*URLConnection urlConnection = new URL("http://localhost:8192/Service/VinciService").openConnection();
      HttpURLConnection connection=(HttpURLConnection)urlConnection;
      //connection.setRequestMethod("POST");
      connection.setDoOutput(true);
      //connection.connect();
      OutputStream os = connection.getOutputStream();
  
      // Post the request file.
      FileInputStream fis = new FileInputStream(testRootJbi+"/testVinciMessage5.xml");
      
      //Buffer
      byte[] buf = new byte[256];
      for (int c = fis.read(buf); c != -1; c = fis.read(buf)) {
        os.write(buf,0,c);
      }
      os.close();
      fis.close();
      log.info("message sent");
      // Read the response.
      BufferedReader in = new BufferedReader(new InputStreamReader(connection.getInputStream()));
      String inputLine;
      while ((inputLine = in.readLine()) != null) {
          log.info(inputLine);
      }
      in.close();
      log.info("finished");*/
    }
    catch (Exception e){
      e.printStackTrace();
      log.error("errore durenate il test: "+e);
      fail(e.getMessage());
    }
  }  

}
