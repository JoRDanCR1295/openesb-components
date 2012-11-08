/*******************************************************************************
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 ******************************************************************************/
package it.imolinfo.jbi4corba.test.servant.testprovideruniontypes;

import it.imolinfo.jbi4corba.test.testprovideruniontypes.ComplexStruct1;
import it.imolinfo.jbi4corba.test.testprovideruniontypes.EchoUnionTypes;

import it.imolinfo.jbi4corba.test.testprovideruniontypes.EchoUnionTypesHelper;
import it.imolinfo.jbi4corba.test.testprovideruniontypes.EchoUnionTypesPOA;

import it.imolinfo.jbi4corba.test.testprovideruniontypes.FirstUnion;
import it.imolinfo.jbi4corba.test.testprovideruniontypes.SecondUnion;
import it.imolinfo.jbi4corba.test.testprovideruniontypes.ThirdUnion;
import java.io.InputStream;
import java.util.Properties;
import java.util.logging.Logger;
import org.omg.CORBA.ORB;
import org.omg.CosNaming.NameComponent;
import org.omg.CosNaming.NamingContextExt;
import org.omg.CosNaming.NamingContextExtHelper;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAHelper;

/**
 * This class is the corba servant used to manage the 'UnionTypes.idl'
 */
public class EchoUnionTypesImpl extends EchoUnionTypesPOA {

  /**
   * logger.
   */
  private static Logger log = Logger.getLogger(EchoUnionTypesImpl.class.getName());
  //LogFactory.getLog(EchoImpl.class);
  /**
   * main first argument is the port (host is supposed to be localhost) second argument is
   * daemon=true/false optional, default false.
   * If daemon is true the servan starts as daemon, useful for integration tests
   */
  public static void main(String[] args) throws Exception {
    String propertyFile = args[0];

    boolean daemon = args.length > 1 ? "daemon=true".equals(args[1]) : false;
    startCorbaServant(daemon, propertyFile);

  }

  private static void startOrbd(final String port) {
    Thread orbdThread = new Thread(new Runnable() {

      public void run() {
        log.info("starting orbd on port: " + port);
        com.sun.corba.se.impl.activation.ORBD.main(new String[]{"-ORBInitialPort", port, "-ORBInitialHost", "localhost"});
      }
    });
    orbdThread.setDaemon(true);
    orbdThread.start();
    log.info("orbd launched");
  }

  private static void startCorbaServant(final boolean daemon, final String orbPropertyFile) {
    Thread servantThread = new Thread(new Runnable() {

      public void run() {
        try {
          // create and initialize the ORB                    
          log.info("loading orb.properties: " + orbPropertyFile);

          InputStream is = this.getClass().getResourceAsStream("/" + orbPropertyFile);
          log.info("input stream: " + is);
          Properties props = new Properties();
          props.load(is);

          log.info("launching orb with properties: " + props);

          ORB orb = ORB.init((String[]) null, props);

          // get reference to rootpoa & activate the POAManager
          POA rootpoa = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
          rootpoa.the_POAManager().activate();

          // create servant and register it with the ORB
          EchoUnionTypesImpl helloImpl = new EchoUnionTypesImpl();
          log.info("EchoImpl ..." + helloImpl);

          // get object reference from the servant
          org.omg.CORBA.Object ref = rootpoa.servant_to_reference(helloImpl);
          EchoUnionTypes href = EchoUnionTypesHelper.narrow(ref);

          if (daemon) {
            startOrbd(props.getProperty("orbd.port"));
            Thread.currentThread().sleep(2000);
          }

          // get the root naming context
                    // NameService invokes the name service
          org.omg.CORBA.Object objRef = orb.resolve_initial_references("NameService");

          // Use NamingContextExt which is part of the Interoperable
                    // Naming Service (INS) specification.
          NamingContextExt ncRef = NamingContextExtHelper.narrow(objRef);

          // bind the Object Reference in Naming
          String name = "EchoUnionTypes";
          NameComponent[] path = ncRef.to_name(name);
          ncRef.rebind(path, href);
          log.info("EchoImpl - echoref rebindato: " + ncRef);

          log.info("EchoServer ready and waiting ...");

          // wait for invocations from clients
          orb.run();
        } catch (Exception e) {
          log.severe("ERROR: " + e);
          e.printStackTrace(System.out);
        }
        log.info("EchoServer Exiting ...");
      }
    });
    servantThread.setDaemon(daemon);
    servantThread.start();
  }
  // ==========================================
    //                  The operations in the IDL
    // ==========================================
  public String echo(String arg0) {
    log.info("echo(String) - message received: " + arg0);
    return arg0;
  }
  
  public String echo1 (it.imolinfo.jbi4corba.test.testprovideruniontypes.ThirdUnion msg)
  {
      try
      {
          log.info("echo1 - message received: altro - " + msg.altro());
         return String.valueOf(msg.altro());
      }
      catch(org.omg.CORBA.BAD_OPERATION ex)
      {
          
      }
      
      try
      {
           log.info("echo1 - message received: primo - " + msg.primo());
         return String.valueOf(msg.primo());
      }
      catch(org.omg.CORBA.BAD_OPERATION ex)
      {
          
      }
      
      try
      {
           log.info("echo1 - message received: secondo - " + msg.secondo());
         return String.valueOf(msg.secondo());
      }
      catch(org.omg.CORBA.BAD_OPERATION ex)
      {
          
      }
      try
      {
          FirstUnion firuns[] = msg.third();
          String result = "";
          for (FirstUnion fu : firuns)
          {
              try
              {
                   log.info("echo1 - message received: third[] - alphanumeric " + fu.alfanumeric());
                 result += ", " +  fu.alfanumeric();
              }
              catch(org.omg.CORBA.BAD_OPERATION ex)
              {

              }
              
              try
              {
                   log.info("echo1 - message received: third[] - numeric " + fu.numeric());
                 result += ", " +  fu.numeric();
              }
              catch(org.omg.CORBA.BAD_OPERATION ex)
              {

              }
              try
              {
                   log.info("echo1 - message received: third[] - two_format " + fu.two_format());
                 result += ", " +  fu.two_format();
              }
              catch(org.omg.CORBA.BAD_OPERATION ex)
              {

              }
          }
           log.info("echo1 - message received: third - " + result);
         return String.valueOf(result);
      }
      catch(org.omg.CORBA.BAD_OPERATION ex)
      {
          
      }
      return "Received Empty Union";
     }
  public it.imolinfo.jbi4corba.test.testprovideruniontypes.ThirdUnion echo2 (it.imolinfo.jbi4corba.test.testprovideruniontypes.SecondUnionHolder msg)
  {
      SecondUnion su = msg.value;
      
       try
      {
          log.info("echo2 - message received: alfanumeric - " + su.alfanumeric());
          
          su.alfanumeric("Received message: " + su.alfanumeric());
          
          ThirdUnion tu = new ThirdUnion();
          tu.altro(true);
          
         return tu;
      }
        catch(org.omg.CORBA.BAD_OPERATION ex)
      {
          
      }
        try
      {
          log.info("echo2 - message received: alfanumeric - " + su.numeric());
          
          ThirdUnion tu = new ThirdUnion();
          tu.primo(12);
          
          su.numeric(su.numeric() + 1);
          
         return tu;
      }
       
      catch(org.omg.CORBA.BAD_OPERATION ex)
      {
          
      }
      
        try
      {
          log.info("echo2 - message received: alfanumeric - " + su.two_format());
          
          ThirdUnion tu = new ThirdUnion();
          FirstUnion firuns[] = new FirstUnion[2];
          firuns[0] = new FirstUnion();
          firuns[0].numeric(100);
          firuns[1] = new FirstUnion();
          firuns[1].numeric(101);
          tu.third(firuns);
          
         return tu;
      }
       
      catch(org.omg.CORBA.BAD_OPERATION ex)
      {
          
      }
      
      return null;
  }
    public it.imolinfo.jbi4corba.test.testprovideruniontypes.ComplexStruct1 echo3 (String msg)
    {
        log.info("echo3 - message received: " + msg);
        it.imolinfo.jbi4corba.test.testprovideruniontypes.ComplexStruct1 cs = new it.imolinfo.jbi4corba.test.testprovideruniontypes.ComplexStruct1();
        it.imolinfo.jbi4corba.test.testprovideruniontypes.SecondUnion su = new it.imolinfo.jbi4corba.test.testprovideruniontypes.SecondUnion();
        su.alfanumeric(msg);
        cs.fieldWChar = su;
        
        return cs;
    }
    public String echo4 (it.imolinfo.jbi4corba.test.testprovideruniontypes.ComplexStruct1 msg)
    {
        log.info("echo4 - message received: msg.fieldWChar - " + msg);
        String ms = null;
          try
      {
          
          ms = msg.fieldWChar.alfanumeric();
          
      }
       
      catch(org.omg.CORBA.BAD_OPERATION ex)
      {
          
      }
        
      try
      {
          
          ms = String.valueOf( msg.fieldWChar.numeric());
          
      }
       
      catch(org.omg.CORBA.BAD_OPERATION ex)
      {
          
      }
        
      return ms;
    }
    public String echo5 (it.imolinfo.jbi4corba.test.testprovideruniontypes.ComplexStruct1Holder msg)
    {
         log.info("echo5 - message received: msg.fieldWChar - " + msg.value);
        try
        {
        msg.value.fieldWChar.alfanumeric("Message Received: " + msg.value.fieldWChar.alfanumeric() );
        }
       
      catch(org.omg.CORBA.BAD_OPERATION ex)
      {
          
      }
        
        try
        {
        msg.value.fieldWChar.alfanumeric("Message Received: " + msg.value.fieldWChar.numeric() );
        }
       
      catch(org.omg.CORBA.BAD_OPERATION ex)
      {
          
      }
        
        return "Message received " + msg;
    }
    public it.imolinfo.jbi4corba.test.testprovideruniontypes.ComplexStruct1 echo6 (String msg)
    {
        
        return null;
    }
    public it.imolinfo.jbi4corba.test.testprovideruniontypes.ComplexStruct1[][] echo7 (String msg)
    {
        it.imolinfo.jbi4corba.test.testprovideruniontypes.ComplexStruct1[][] acs = new it.imolinfo.jbi4corba.test.testprovideruniontypes.ComplexStruct1[2][3];
        
        for (int i = 0;i < 2;  i ++)
        {
            for (int j = 0;j < 3;j++)
            {
                SecondUnion su = new SecondUnion();
                su.alfanumeric("element[" + i + "][" + j + "]");
                acs[i][j] = new ComplexStruct1();
                acs[i][j].fieldWChar = su;
            }
        }
        
        return acs;
    }
    public it.imolinfo.jbi4corba.test.testprovideruniontypes.SecondUnion echo8 (String msg) throws it.imolinfo.jbi4corba.test.testprovideruniontypes.EchoUnionException
    {
        if ("exception".equals(msg))
        {
            it.imolinfo.jbi4corba.test.testprovideruniontypes.EchoUnionException ex = new it.imolinfo.jbi4corba.test.testprovideruniontypes.EchoUnionException();
            ThirdUnion tu = new ThirdUnion();
            tu.secondo((short)443);
            ex.reason = tu;
            throw ex;
        }
        else
        {
            it.imolinfo.jbi4corba.test.testprovideruniontypes.SecondUnion su = new it.imolinfo.jbi4corba.test.testprovideruniontypes.SecondUnion();
            su.alfanumeric(msg);
            return su;
        }
        
    }
    public String echo9(it.imolinfo.jbi4corba.test.testprovideruniontypes.ComplexStruct1[][] msg)
    {
        String message = new String();
        
         for (int i = 0;i < msg.length;  i ++)
        {
            for (int j = 0;j < msg[i].length;j++)
            {
              message += msg[i][j].fieldWChar.alfanumeric() + ", ";  
                
            }
        }
        return message;
    }
  
}