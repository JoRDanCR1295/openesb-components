/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)XsltseDescriptor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.transform.descriptor;

import java.io.File;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.management.DeploymentException;

import org.w3c.dom.Document;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.descriptor.JbiDescriptor;
import com.sun.jbi.common.util.EntryRegistry;
import com.sun.jbi.common.xml.XmlUtil;
import com.sun.transform.I18n;
import com.sun.transform.engine.model.ProcessFactory;

/**
 * Represents the XSLT SE endpoint descriptor, which contains configuration
 * for provisioned XSLT service endpoints.
 * 
 * @author Kevan Simpson
 */
public class ProcessDescriptor extends JbiDescriptor {
    private static Logger mLogger = 
    		Logger.getLogger(ProcessDescriptor.class.getName());
//    private static SAXParserFactory mFactory = SAXParserFactory.newInstance();
    
    public static final String DESCRIPTOR_FILE = "transformmap.xml";

    private EntryRegistry<EndpointInfo, TransformEndpoint> mProvidesEndptMap = 
            new EntryRegistry<EndpointInfo, TransformEndpoint>();
    private EntryRegistry<EndpointInfo, TransformEndpoint> mConsumesEndptMap =
            new EntryRegistry<EndpointInfo, TransformEndpoint>();
    private String mTargetNamespace;
    
    
    /**
     * @return the targetNamespace of this descriptor.
     */
    public String getTargetNamespace() {
        return mTargetNamespace;
    }

    /**
     * @param targetNamespace the targetNamespace to set
     */
    protected void setTargetNamespace(String targetNamespace) {
        mTargetNamespace = targetNamespace;
    }

    /**
     * Looks up endpoint entry based on the specified information.
     * @param info The endpoint information.
     * @return an endpoint entry or <code>null</code>.
     */
    public TransformEndpoint lookupEndpointDef(EndpointInfo info) {
        if (info.isProvides()) {
            return mProvidesEndptMap.lookup(info);
        }
        else {
            return mConsumesEndptMap.lookup(info);
        }
    }
    
    /**
     * Registers an endpoint entry with this descriptor.
     * @param endpt The endpoint entry to register.
     */
    public void registerEndpoint(TransformEndpoint endpt) {
    	if (endpt.getInfo().isProvides()) {
    		mProvidesEndptMap.register(endpt.getInfo(), endpt);
    	}
    	else {
    		mConsumesEndptMap.register(endpt.getInfo(), endpt);
    	}
    }

    /**
     * Parses the transformmap configuration file and creates transformation
     * process definitions.
     * 
     * @param rootPath The service unit installation directory.
     * @param factory A <code>ProcessFactory</code> for creating transformation processes.
     * @return a representation of transformation process definitions in the service unit
     * @throws DeploymentException
     */
    public static ProcessDescriptor parse(String rootPath, ProcessFactory factory) 
            throws DeploymentException {
        try {
            TransformmapParser parser = new TransformmapParser(rootPath, factory);
            File file = new File(rootPath, DESCRIPTOR_FILE);
            if (!file.exists()) {
                throw error(null, 
                            I18n.loc("TRANSL-6022: Cannot locate transformation descriptor file: {0}", 
                                     file.getAbsolutePath()));
            }

            Document doc = XmlUtil.readXml(file);
            if (doc == null) {
                return new ProcessDescriptor();
            }
            
            ProcessDescriptor desc = parser.parse(doc.getDocumentElement());
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.fine("TRANSL-3003: Completed parsing transformation descriptor: "+
                             file.getAbsolutePath());
            }

            return desc;
        }
        catch (DeploymentException de) {
            throw de;
        }
        catch (Exception e) {
            throw error(I18n.loc("TRANSL-6001: Failed to parse transformmap descriptor: {0}",
                                 e.getMessage()), 
                        e);
        }
    }

    
    private static DeploymentException error(Exception thrown, String msg) {
        if (thrown == null) {
            mLogger.warning(msg);
            return new DeploymentException(msg);
        }
        else {
            mLogger.log(Level.WARNING, msg, thrown);
            return new DeploymentException(msg, thrown);
        }
    }
}
