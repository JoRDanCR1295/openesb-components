/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4ejb.webservice.generator;

import it.imolinfo.jbi4ejb.Logger;
import it.imolinfo.jbi4ejb.LoggerFactory;
import it.imolinfo.jbi4ejb.exception.ClassGenerationException;
import it.imolinfo.jbi4ejb.exception.EJBDeployException;
import it.imolinfo.jbi4ejb.exception.EJBWSDLGenerationException;
import it.imolinfo.jbi4ejb.jbi.Messages;
import it.imolinfo.jbi4ejb.runtime.ejbproxy.StatelessEJBProxyFactory;

import java.io.File;
import java.io.IOException;
import java.io.ObjectStreamClass;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Set;


/**
 * Utility class to generate the service WSDL.
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
public final class DynamicEJBWSDLGenerator {        

    private static final Logger LOG
        = LoggerFactory.getLogger(DynamicEJBWSDLGenerator.class);
    private static final Messages MESSAGES
    = Messages.getMessages(DynamicEJBWSDLGenerator.class);
    
    /**
     * Private default constructor to avoid instantiation.
     */
    private DynamicEJBWSDLGenerator() {}
    
    /**
     * Creates the WSDL for the remote interface, using the jar passed.
     * 
     * @param remoteInterfaceClassName
     *            The rermote interface class name
     * @param ejbJarPath
     *            the ejb-jar path
     * @param descriptor
     *            The WSDLDescriptor
     * @return the absolute WSDL path name
     * 
     * @throws EJBWSDLGenerationException
     *             If some problem occurs
     */
    public static String generateWSDLFromRemoteInterface(String remoteInterfaceClassName, 
                String ejbJarPath, WSDLDescriptor descriptor) throws EJBWSDLGenerationException {
                
        // Creates the working temp dir
        File tempDir;
        try {
            tempDir = EJBUtils.createTempDir();
        } catch (IOException e) {
        	String msg=MESSAGES.getString("EJB001001_generateWSDLFromRemoteInterface", new Object[]{e.getMessage()});
            LOG.error(msg,e);
            throw new EJBWSDLGenerationException(msg,e);
        }
        String wsdlFileName = tempDir.getAbsolutePath() + File.separatorChar + remoteInterfaceClassName + ".wsdl";
        File interfaceWSDL = WSDLGenerator.createWsdlFromJar(remoteInterfaceClassName, ejbJarPath, wsdlFileName, descriptor, tempDir);
        return interfaceWSDL.getAbsolutePath();
    }
    
    /**
     * Gets the UID from the classes used by the remote interface. 
     * 
     * @param remoteInterface
     *              The remote interface
     * @param ejbJarPath
     *              The complete ejb-jar path
     * @return the classes ID
     * 
     * @throws EJBWSDLGenerationException
     *              If some error occurs
     */
    @SuppressWarnings("unchecked")
    public static Hashtable getClassesID(String remoteInterface, String ejbJarPath) throws EJBWSDLGenerationException {
        
        Hashtable classesID = new Hashtable();

        File tempDir;
        try {
            tempDir = File.createTempFile("EJBCLASSES_", null);
        } catch (IOException e) {
        	String msg=MESSAGES.getString("EJB001002_getClassesID", new Object[]{e.getMessage()});
            LOG.error(msg,e);
            throw new EJBWSDLGenerationException(msg,e);
        } 
        tempDir.delete();
        tempDir.mkdir();

        try {
            JarUtil.unjar(new File(ejbJarPath), tempDir);
        } catch (IOException e) {
        	String msg=MESSAGES.getString("EJB001002_getClassesID", new Object[]{e.getMessage()});
            LOG.error(msg,e);
            throw new EJBWSDLGenerationException(msg,e);
        }

        String remoteInterfaceFileName = remoteInterface.replace('.',File.separatorChar);

        LOG.debug("remoteInterfaceFileName: " + remoteInterfaceFileName);
        List<File> list = new ArrayList<File>();
        list.add(new File(tempDir.getAbsolutePath() + File.separatorChar + remoteInterfaceFileName));

        // Find the classes used by the remote interface (with recursion).
        Set<Class> classesToSerialize;
        try {
            classesToSerialize = Util.findClassUsed(tempDir.getAbsolutePath(), list);
        } catch (ClassGenerationException e) {
        	String msg=MESSAGES.getString("EJB001002_getClassesID", new Object[]{e.getMessage()});
            LOG.error(msg,e);
            throw new EJBWSDLGenerationException(msg,e);
        }

        Iterator iter = classesToSerialize.iterator();
        while (iter.hasNext()) {
            Class classToSerialize = (Class)iter.next();
            LOG.debug(classToSerialize.getName());
                        
            ObjectStreamClass objectStreamClass = ObjectStreamClass.lookup(classToSerialize);
                        
            long uid = objectStreamClass.getSerialVersionUID();
             
/*
            // With com.sun.corba.se.impl.io.ObjectStreamClass;
            long uid = ObjectStreamClass.getSerialVersionUID(classToSerialize);

            if (uid == 0L) {
                uid = ObjectStreamClass.getActualSerialVersionUID(classToSerialize);
            }
*/            

            LOG.debug(classToSerialize.getName() + " uid: " + uid);            
            classesID.put(classToSerialize.getName() , Long.valueOf(uid));
        }            

        return classesID;
        
    }        
    
    
}
