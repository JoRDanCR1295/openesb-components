 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.utils;

import java.io.File;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;

/**
 * An utility  for generation of java from IDL
 * 
 * 
 * @author <a href="mailto:lacquaviva@imolinfo.it">Luca Acquaviva</a>
 */
public class HelperIDLJUtil {
    
     /**
     * Logger.
     */
    private static final Logger LOG = LoggerFactory.getLogger(HelperIDLJUtil.class);
    
    
    
    /**
     * idlj generates Java bindings from a given IDL file.
     *
     * Syntax: idlj [ options ] idl-file
     *
     * options:
     * -fall    generate both client and server-side bindings.
     * -td        emit files to a directory other than the current directory
     * -i        include directory
     *
     * @param    targetdir    The directory where the java files will be placed.
     * @param    includedir   The directory to find include files.
     * @param    idlFilename The IDL file to compile.
     *
     * @see    http://java.sun.com/j2se/1.5.0/docs/guide/rmi-iiop/toJavaPortableUG.html
     */
    public static void idlj(final String targetdir,final String includedir,final String idlFilename) {
        //Added for the creation of idl withoud Modules
        File tdir=new File(targetdir);
        // this is necessary for the correct generation from an idl without module
        if(!tdir.exists()){
        	
        		tdir.mkdir();
			
        }
        com.sun.tools.corba.se.idl.toJavaPortable.Compile.main(
                new String[]{"-emitAll",
                    "-fall",
                    "-td",
                    targetdir, // workdirsrc
                    "-i",
                    includedir,
                    idlFilename
                });

        LOG.debug("<<<<< idlj - end");
    }

    /**
     * idlj generates Java bindings from a given IDL file.
     *
     * Syntax: idlj [ options ] idl-file
     *
     * options:
     * -fallTIE		generate both client and server-side bindings and xxxPOATie class.
     * -td			emit files to a directory other than the current directory
     * -i        	include directory
     *
     * @param    targetdir    The directory where the java files will be placed.
     * @param    includedir   The directory to find include files.
     * @param    idlFilename The IDL file to compile.
     *
     * @see    http://java.sun.com/j2se/1.5.0/docs/guide/rmi-iiop/toJavaPortableUG.html
     */
    public static void idljPoaTie(final String targetdir,final String includedir,final String idlFilename) {
        LOG.debug(">>>>> idlj - begin");

        com.sun.tools.corba.se.idl.toJavaPortable.Compile.main(
                new String[]{"-emitAll",
                    "-fallTIE",
                    "-td",
                    targetdir, // workdirsrc
                    "-i",
                    includedir,
                    idlFilename
                });

        LOG.debug("<<<<< idlj - end");
    }
    

}
