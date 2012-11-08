/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4ejb.webservice.generator.bcm;

import it.imolinfo.jbi4ejb.Logger;
import it.imolinfo.jbi4ejb.LoggerFactory;
import it.imolinfo.jbi4ejb.exception.ClassGenerationException;
import it.imolinfo.jbi4ejb.webservice.generator.EJBUtils;

import java.util.ArrayList;
import java.util.List;

import org.objectweb.asm.ClassAdapter;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.MethodVisitor;


/**
 * Removes the Remote/EJBObject interfaces and the throws RemoteException.
 * 
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
public class RemoveEJBInterfaceAdapter extends ClassAdapter {

    /** The logger. */
    private static final Logger LOG
    = LoggerFactory.getLogger(RemoveEJBInterfaceAdapter.class); 
    
    // The classes dir name.
    private String classesDirName = null;

    /**
     * Constructor.
     * 
     * @param arg0
     *            The <code>ClassVisitor</code>
     */
    public RemoveEJBInterfaceAdapter(ClassVisitor arg0, String classesDirName) {
        super(arg0);      
        this.classesDirName = classesDirName;
    }

    /**
     * Removes the "implements Remote, EJBObject" clause. 
     * 
     * @param version
     *          The versione
     * @param access
     *          the access modifier
     * @param name
     *          The class name
     * @param signature
     *          The signatur
     * @param superName
     *          The superclass name
     * @param interfaces
     *          The interfaces array.
     * 
     * @see org.objectweb.asm.ClassAdapter#visit(int, int, java.lang.String,
     *      java.lang.String, java.lang.String, java.lang.String[])
     */
    public void visit(int version, int access, String name, String signature,
            String superName, String [] interfaces) {

        LOG.debug("Removing java/rmi/Remote from the class: " + name);
        List<String> newInterfaces = new ArrayList<String>();

        if (interfaces != null) {
            for (int i = 0; i < interfaces.length; i++) {                
                String newInterface = interfaces[i];
                // If the exception is NOT a Remote, adds it.
                if (!(newInterface.equals("java/rmi/Remote")) && (!newInterface.equals("javax/ejb/EJBObject"))) {
                    newInterfaces.add(interfaces[i]);
                }
            }
        } 

        // Recursive call: every interface can throw RemoteException
        for (int i = 0; i < newInterfaces.size(); i++) {
            String interfaceClassname  = newInterfaces.get(i).replace('/', '.');
            try {
                EJBUtils.removeEJBRemoteInterface(interfaceClassname, classesDirName);
            } catch (ClassGenerationException e) {
                // TODO i18n
                String msg = "Error in removing java.rmi.Remote from interface " + e.getMessage();
                LOG.warn(msg, e);
            }
        }
        
        String[] newInterfacesArray = newInterfaces.toArray(new String[0]); 

        super.visit(version, access, name, signature, superName, newInterfacesArray);
    }


    
    
    /**
     * Visit the method, tremoving the "throws RemoteException, EJBException" clause.
     * 
     * @param access
     *          The access modifier
     * @param name
     *          The method name
     * @param desc
     *          The description
     * @param signature
     *          The signature
     * @param exceptions    
     *          The exceptions array
     * 
     * @return
     *      The MethodVisitor
     * 
     * @see org.objectweb.asm.ClassAdapter#visitMethod(int, java.lang.String,
     *      java.lang.String, java.lang.String, java.lang.String[])
     */
    public MethodVisitor visitMethod(int access, String name, String desc, String signature, String[] exceptions) {

        List<String> newExceptions = new ArrayList<String>();

        if (exceptions != null) {
            for (int i = 0; i < exceptions.length; i++) {                
                String exception = exceptions[i];
                // If the exception is NOT a RemoteException, adds it.
                if (!(exception.equals("java/rmi/RemoteException")) && (!(exception.equals("javax/ejb/EJBException")))) {
                    newExceptions.add(exceptions[i]);
                } else {
                    LOG.debug("Removing java/rmi/RemoteException from the method: " +  name);
                }
            }
        }
        String[] newExceptionsArray = newExceptions.toArray(new String[0]);                 
        return super.visitMethod(access, name, desc, signature, newExceptionsArray);
    }

}


