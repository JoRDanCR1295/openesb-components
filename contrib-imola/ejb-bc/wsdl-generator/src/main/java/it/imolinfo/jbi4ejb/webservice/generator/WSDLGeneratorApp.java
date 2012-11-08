/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4ejb.webservice.generator;

import it.imolinfo.jbi4ejb.exception.EJBWSDLGenerationException;

import java.io.File;
import java.io.IOException;
import java.util.Properties;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.GnuParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * WSDL Generation application.
 * The parameters are:
 * <UL>
 *      <LI>The interface class name
 *      <LI>The jar path
 *      <LI>The WSDL file name to produce (optional, deduced from the interface name)
 *      <LI>The corbaname (optional, can be filled later but produces a warning)
 *      <LI>The Application server (optional, default to Glassfish 2.0)
 * </UL>
 * 
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
public class WSDLGeneratorApp 
{
    
    /** Logger. */
    private static final Log LOG = LogFactory.getLog(WSDLGeneratorApp.class);   
    
    /** Websphere constants */ 
    public static final String WEBSPHERE_6 = "websphere6";
    
    /** Glassfish constants */
    public static final String GLASSFISH_v2 = "glassfish2";
        
    /** Jboss constants */
    public static final String JBOSS_V4 = "jboss4";
    
    /** Glassfish constants */
    public static final String JACORB = "jacorb"; 
    
    public static final String usage = "-i <ejb-interface> -j <jar-path>";
    public static final String header = "\nExtended WSDL Generator for the Jbi4Ejb Binding component";
    public static final String footer =  "\nCopyright (c) 2005, 2006, 2007 Imola Informatica. \n" +
                     "All rights reserved. This program and the accompanying materials \n" +
                     "are made available under the terms of the LGPL License v2.1" +
                     "which accompanies this distribution, and is available at" +
                     "http://www.gnu.org/licenses/lgpl.html\n";   
                
    
    /**
     * The main entry point
     * 
     * @param args
     *            the arguments
     */
    public static void main( String[] args ) {
                
        // create the options parser
        CommandLineParser parser = new GnuParser();
        CommandLine line = null;
        Options options = createOptions();
        try {
            // parses the command line arguments
            line = parser.parse(options, args );
        }   catch (ParseException exp ) {             
            System.err.println( "Options parsing failed.  Reason: " + exp.getMessage() );
            System.exit(-1);
        }        

        // Gets oll the option values
        Option helpOpt = getOption("help", line);
        Option interfaceOpt = getOption("interface", line);
        Option jarPathOpt = getOption("jar-path", line);
        Option wsdlOpt = getOption("wsdl", line);
        Option corbanameOpt = getOption("corbaname", line);
        Option appserverOpt = getOption("appserver", line);        
        
        if (helpOpt != null || line.getOptions().length == 0) {
            printHelp(options);
        } 
                          
        if (interfaceOpt == null) {
            System.err.println("Error: The ejb interface name is required!");
            System.err.println("usage:" + usage);      
            System.exit(0);     
        }
        
        if (jarPathOpt == null) {
            System.err.println("Error: The ejb-jar path is required!");
            System.err.println("usage:" + usage);
            System.exit(0);
        }        
        
        String interfaceClassName = interfaceOpt.getValue();
        String jarPath = jarPathOpt.getValue();
        String wsdlFileName = null;
        String corbaname = null;
        String appServer = null;
        if (wsdlOpt != null) {
            wsdlFileName = wsdlOpt.getValue();
        } else {
            // The wsdl file name is the className + ".wsdl"
            wsdlFileName = interfaceClassName.substring(interfaceClassName.lastIndexOf('.') + 1, interfaceClassName.length());
            wsdlFileName = wsdlFileName  +".wsdl";
        }        
        if (corbanameOpt != null) {
            corbaname = corbanameOpt.getValue();
        } else {
            corbaname ="corbaname-to change";
        }
        if (appserverOpt != null) {
            appServer = appserverOpt.getValue();
        } else {
            appServer = GLASSFISH_v2;
        }
        System.out.println("\nGenerating WSDL with parameters: \n");
        System.out.println("Interface: " + interfaceClassName);
        System.out.println("Jar path: " + jarPath);
        System.out.println("WSDL path: " + wsdlFileName);
        System.out.println("corbaname: " + corbaname);
        System.out.println("Application Server: " + appServer +"\n");
        
        // Generates the wsdl:        
        try {
            generateWSDL(interfaceClassName, jarPath, wsdlFileName, corbaname, appServer);
        } catch (WSDLGenerationAppException e) {
            System.err.println("Error: " + e.getMessage());
            System.err.println("WSDL NOT generated");
        }
    }
    
    /**
     * Creates the options.
     * 
     * @return the options
     */
    private static Options createOptions() {
        Options options = new Options();
        
        Option help = new Option("h", "help", false, "print this message" );
        
        // adds the remote interface        
        Option remoteInterfaceOption = new Option("i", "interface", true, "the ejb remote interface");
        remoteInterfaceOption.setArgName("ejb-interface");         
        
        // Adds the ejb jar path
        Option jarPathOption = new Option("j","jar-path", true, "the ejb jar path");
        jarPathOption.setArgName("ejb-jar-path");
        
        // Adds the wsdl path
        Option wsdlOption = new Option("w","wsdl", true, "the wsdl file name (optional)");
        wsdlOption.setArgName("wsdl-file-name");
        
        // Adds the corbaname
        Option corbanameOption = new Option("c","corbaname", true, "the ejb corbaname (optional)");
        corbanameOption.setArgName("corbaname");        

        // Adds the appserver option
        Option appserverOption = new Option("a","appserver", true, "the app server name (websphere6/glassfish2/jboss4)");
        appserverOption.setArgName("appserver");
                           
        options.addOption(help);
        options.addOption(remoteInterfaceOption);
        options.addOption(jarPathOption);
        options.addOption(wsdlOption);
        options.addOption(corbanameOption);
        options.addOption(appserverOption);
        
        return options;
    }
    
    /**
     * Gets the option from the line
     * 
     * @param logOptionName
     * @param line
     * 
     * @return the option
     */
    private static Option getOption(String logOptionName, CommandLine line) {
        Option opt = null;
        Option[] opts = line.getOptions();
        for (int i = 0; i < opts.length; i++) {
            if (opts[i].getLongOpt().equals(logOptionName)) {
                opt = opts[i];
            }            
        }
        return opt;
    }
    
    /**
     * Prints the help.
     * 
     * @param options
     */
    private static void printHelp(Options options) {
        // Prints out the help
        HelpFormatter formatter = new HelpFormatter();                    
        formatter.printHelp(usage, header, options, footer);
        System.exit(0);
    }
    

    
    /**
     * Generate the WSDL from the remote EJB interface.
     * 
     * @param interfaceClassName
     *          The interface name
     * @param jarPath
     *          The jar path
     * @param WSDLfileName
     *          The WSDL name
     * @param corbaName
     *          The corba name
     * @param appServer
     *          The application server (to add the default orb settings)
     * 
     * @throws WSDLGenerationAppException
     */
    private static void generateWSDL(String interfaceClassName, String jarPath, String wsdlfileName, String corbaName, String appServer) throws WSDLGenerationAppException {
        File tempDir = createTempDir();
        WSDLDescriptor wsdlDescription = new WSDLDescriptor(corbaName, WSDLDescriptor.CORBANAME_LOCALIZATION_TYPE);
        // Sets the app server orb properties
        wsdlDescription.setOrbProperties(getOrbProperties(appServer));
        
        // Creates the WSDL
        try {
            File wsdlFile = WSDLGenerator.createWsdlFromJar(interfaceClassName, jarPath, wsdlfileName, wsdlDescription, tempDir);
            LOG.info("WSDL successfully created: " + wsdlFile.getAbsolutePath());
        } catch (EJBWSDLGenerationException e) {
            String msg = "Error in generating the WSDL:" + e.getMessage();
            LOG.error(msg);
            throw new WSDLGenerationAppException(msg, e);
        } finally {
            tempDir.delete();
        }
    }
    
    /**
     * Creates a temporary directory (for unit testing pourpose).
     * @return
     */
    private static File createTempDir()  {
        // Creates the working temp dir
        File tempDir = null;
        try {
            tempDir = EJBUtils.createTempDir();
        } catch (IOException e) {
            LOG.error(e.getMessage());
            e.printStackTrace();
        }
        return tempDir;
    }
    
    /**
     * Gets the default orb properties.
     * 
     * @param asType the types (see the app server constants
     * 
     * @return the orb properties
     */
    private static Properties getOrbProperties(String asType) {
        
        Properties props = new Properties();
        
        if (WEBSPHERE_6.equalsIgnoreCase(asType)) {                                    
            // IBM Websphere 6 properties            
            props.setProperty("org.omg.CORBA.ORBClass", "com.ibm.CORBA.iiop.ORB");             
        } else if (GLASSFISH_v2.equalsIgnoreCase(asType)) {
            props.setProperty("org.omg.CORBA.ORBClass", "com.sun.corba.ee.impl.orb.ORBImpl");
        } else if (JACORB.equalsIgnoreCase(asType)) {
            // Jacorb properties            
            props.setProperty("org.omg.CORBA.ORBClass", "org.jacorb.orb.ORB");
            props.setProperty("org.omg.CORBA.ORBSingletonClass", "org.jacorb.orb.ORBSingleton");             
        } else if (JBOSS_V4.equalsIgnoreCase(asType)) {
            // Jboss properties
            props.setProperty("org.omg.CORBA.ORBClass", "org.jacorb.orb.ORB");
            props.setProperty("org.omg.CORBA.ORBSingletonClass", "org.jacorb.orb.ORBSingleton");
        }
        return props;
    }    
}
