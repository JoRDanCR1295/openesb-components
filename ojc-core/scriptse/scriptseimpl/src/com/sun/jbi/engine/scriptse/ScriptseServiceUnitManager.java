/* *************************************************************************
 *
 *          Copyright (c) 2005, SeeBeyond Technology Corporation,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 ***************************************************************************/
package com.sun.jbi.engine.scriptse;


import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.component.endpoint.EndpointManager;
import com.sun.jbi.component.lifecycle.impl.AbstractServiceUnitManager;
import com.sun.jbi.crl.mep.ManagerContext;

import java.util.logging.Level;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.Enumeration;
import java.io.FileFilter;
import java.io.File;
import java.io.ByteArrayOutputStream;
import java.io.ByteArrayInputStream;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.io.IOException;

import javax.jbi.component.ComponentContext;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.management.DeploymentException;
import com.sun.jbi.engine.scriptse.process.ParseXsds;


/**
 * ScriptSE implementation of {@link ServiceUnitManager}.
 *
 * @author Prashanth B.R
 */
public class ScriptseServiceUnitManager extends AbstractServiceUnitManager {
    //    private StatusProviderHelper mStatusProviderHelper;
    /**
     * DOCUMENT ME!
     *
     * @param componentCtx
     * @param emgr
     */
    public ScriptseServiceUnitManager(ManagerContext componentCtx, EndpointManager emgr) {
        super(componentCtx, emgr);
    }

    //    void initialize(StatusProviderHelper statusProviderHelper) {
    //        mStatusProviderHelper = statusProviderHelper;
    //    }
    /**
     *
     * @see com.sun.jbi.component.lifecycle.impl.AbstractServiceUnitManager#init(java.lang.String,
     *      java.lang.String)
     */
    public void init(String serviceUnitName, String serviceUnitRootPath)
        throws DeploymentException {
        log().info("Initializing service unit " + serviceUnitName);
        log().info("Initializing service unit:: ServiceUnitRoot " + serviceUnitRootPath);

        super.init(serviceUnitName, serviceUnitRootPath);

        //Trying to intialize the JAR FILE NAME HERE..
        if (log().isLoggable(Level.INFO)) {
            log().info(
                "Initialized service unit " + serviceUnitName + " serviceUnitRootPath: " +
                serviceUnitRootPath + " successfully."
            );
        }
    }
    public String deploy(String serviceUnitName, String serviceUnitRootPath) throws DeploymentException {

    	try{
    		ParseXsds parse = new ParseXsds();
    		parse.parseForXsds(serviceUnitRootPath);
    	   }catch(Exception e){

    	   }
    	return super.deploy(serviceUnitName, serviceUnitRootPath);
    }

 }
