/*******************************************************************************
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 ******************************************************************************/
package it.imolinfo.jbi4corba.test.orbd;

import java.io.InputStream;
import java.util.Properties;
import java.util.logging.Logger;
import org.omg.CORBA.ORB;

/**
 * This class run orbd
 */
public class Orbd {

  /**
   * logger.
   */
  private static Logger log = Logger.getLogger(Orbd.class.getName());
  //LogFactory.getLog(EchoImpl.class);
  
  //private static boolean enable = false;

  /**
   * main first argument is the port (host is supposed to be localhost) second argument is
   * daemon=true/false optional, default false.
   * If daemon is true the orbd starts as daemon, useful for integration tests
   */
  public static void main(String[] args) throws Exception {
    String propertyFile = args[0];

    boolean daemon = args.length > 1 ? "daemon=true".equals(args[1]) : false;
    start(daemon, propertyFile);

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

  private static void start(final boolean daemon, final String orbPropertyFile) {

    //log.info("Orbd enable value: [ " + Orbd.isEnable()+" ]");
    //if (!Orbd.isEnable()) {
      Thread orbThread = new Thread(new Runnable() {

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

            startOrbd(props.getProperty("orbd.port"));
            //Orbd.setEnable(true);
            Thread.currentThread().sleep(2000);

            // wait for invocations from clients
            orb.run();
          } catch (Exception e) {
            log.severe("ERROR: " + e);
            e.printStackTrace(System.out);
          }
          log.info("ORBD Exiting ...");
        }
      });
      orbThread.setDaemon(daemon);
      orbThread.start();
  /*  
  } else {
      log.info("ORB is already enable");
    }*/
  }
/*
  public static boolean isEnable() {
    return Orbd.enable;
  }

  public static void setEnable(boolean b) {
    Orbd.enable = b;
  }
 * */
}