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
 * @(#)AbstractTestCase.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.test.bpelpersist;

import java.io.File;
import java.io.FileReader;
import java.io.Reader;
import java.net.URI;
import java.net.URL;
import java.rmi.server.UID;
import java.sql.Connection;
import java.util.Properties;

import javax.wsdl.Definition;
import javax.wsdl.xml.WSDLReader;
import javax.xml.namespace.QName;

import com.sun.jbi.engine.bpel.core.bpel.connection.ConnectionProperties;
import junit.framework.TestCase;

import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.persist.StateFactory;
import com.sun.jbi.engine.bpel.core.bpel.persist.StateManager;
import com.sun.jbi.engine.bpel.core.bpel.persist.impl.StateImpl;
import com.sun.jbi.engine.bpel.core.test.common.DummyTxManagerAndDataSource;
import com.sun.jbi.engine.bpel.core.test.common.OneTimesetUp;
import com.sun.jbi.engine.bpel.core.test.common.Utility;
import com.sun.wsdl4j.ext.WSDL4JExt;
import com.sun.bpel.xml.wsdl.WSDLDocument;
import com.sun.bpel.xml.wsdl.impl.WSDLDocumentImpl;

/**
 * @author Sun Microsystems
 */
public abstract class AbstractTestCase extends TestCase {
    String mEngId;
    protected Engine mEng;
    protected Properties mConnProp;
    protected StateManager mStateMgr;
    protected int dbType = -1;

    public AbstractTestCase(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        mEng = OneTimesetUp.getSetUp().getEngine();
        mEngId = mEng.getId();
        mConnProp = OneTimesetUp.getSetUp().getConnectionProperties();
        mStateMgr = mEng.getStateManager();
    }

    protected void tearDown() throws Exception {
        // remove BPELs and inserted instance data??
    }

    protected Connection getConnection() throws Exception {
        DummyTxManagerAndDataSource txMgr = new DummyTxManagerAndDataSource(mConnProp);
        String dbName = txMgr.getConnection().getMetaData().getDatabaseProductName();
			// DO we need to check for the supported version also ?
			if (dbName.equalsIgnoreCase("derby")) {
				dbType = ConnectionProperties.DERBY_DB.intValue();
			} else if (dbName.equalsIgnoreCase("oracle")) {
				dbType = ConnectionProperties.ORCL_DB.intValue();
			} else if (dbName.equalsIgnoreCase("mysql")) {
				dbType = ConnectionProperties.MYSQL_DB.intValue();
			} else if (dbName.equalsIgnoreCase("postgresql")) {
				dbType = ConnectionProperties.POSTGRES_DB.intValue();
			}
        return txMgr.getNonTxConnection();
    }

    protected RBPELProcess loadBPELModel(String fileName) {
        String filePath = "bpel/" + fileName;

        RBPELProcess bProc = null;

        try {
            URL url = getClass().getResource(filePath);
            bProc = Utility.loadBPEL(url);
        } catch (Throwable t) {
            fail(" failed to parse the file " + t.getMessage());
        }

        return bProc;
    }
    
    protected WSDLDocument loadWSDL(String location) throws Exception {
        URL wsdlURL = getClass().getResource(location);
        URI uri = wsdlURL.toURI();
        WSDLReader rdr = WSDL4JExt.newWSDLReader(null);
        Definition wsdlDef = rdr.readWSDL(uri.toString());
        WSDLDocument doc = new WSDLDocumentImpl(uri.toString(), wsdlDef);     
        return doc;
    }
    
    protected StateImpl createState(QName bpelId) {
        String bpId = new UID().toString();

        return (StateImpl) StateFactory.getStateFactory().createState(mEng, bpelId, bpId);
    }
}
