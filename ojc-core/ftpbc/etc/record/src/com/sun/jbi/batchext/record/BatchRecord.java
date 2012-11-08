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
 * @(#)BatchRecord.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/******************************************************************************
 * Copyright © 2006 Sun Microsystems, Inc., 4150 Network Circle, Santa Clara,
 * California 95054, U.S.A. All rights reserved. Sun Microsystems, Inc. has
 * intellectual property rights relating to technology embodied in the product
 * that is described in this document. In particular, and without limitation,
 * these intellectual property rights may include one or more of the U.S. patents
 * listed at http://www.sun.com/patents and one or more additional patents or
 * pending patent applications in the U.S. and in other countries. THIS PRODUCT
 * CONTAINS CONFIDENTIAL INFORMATION AND TRADE SECRETS OF SUN MICROSYSTEMS, INC.
 * USE, DISCLOSURE OR REPRODUCTION IS PROHIBITED WITHOUT THE PRIOR EXPRESS WRITTEN
 * PERMISSION OF SUN MICROSYSTEMS, INC.U.S. Government Rights - Commercial
 * software.  Government users are subject to the Sun Microsystems, Inc. standard
 * license agreement and applicable provisions of the FAR and its supplements.
 * Use is subject to license terms.  This distribution may include materials
 * developed by third parties. Sun, Sun Microsystems, the Sun logo, Java
 * Composite Application Platform Suite,  SeeBeyond, eGate, eInsight, eVision, eTL,
 * eXchange, eView, eIndex, eBAM and  eWay are trademarks or registered trademarks of
 * Sun Microsystems, Inc. in the U.S. and other countries. All SPARC trademarks are
 * used under license and are trademarks or registered trademarks of SPARC
 * International, Inc. in the U.S. and other countries. Products bearing SPARC
 * trademarks are based upon architecture developed by Sun Microsystems, Inc.
 * UNIX is a registered trademark in the U.S. and other countries, exclusively
 * licensed through X/Open Company, Ltd. This product is covered and controlled by
 * U.S. Export Control laws and may be subject to the export or import laws in
 * other countries.  Nuclear, missile, chemical biological weapons or nuclear
 * maritime end uses or end users, whether direct or indirect, are strictly
 * prohibited.  Export or reexport to countries subject to U.S. embargo or to
 * entities identified on U.S. export exclusion lists, including, but not limited
 * to, the denied persons and specially designated nationals lists is strictly
 * prohibited.
 **/
package com.sun.jbi.batchext.record;

import com.sun.jbi.batchext.BatchCFGException;
import com.sun.jbi.batchext.BatchException;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Properties;

//~import com.stc.connector.batchadapter.alerts.batch.BatchAlertUtil;
//~import com.stc.connector.batchadapter.alerts.batch.BatchRecordAlertCodes;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.internationalization.Messages;

//~import com.stc.eventmanagement.NotificationEvent;
import com.sun.jbi.batchext.streaming.InputStreamAdapter;
import com.sun.jbi.batchext.streaming.OutputStreamAdapter;
import com.sun.jbi.batchext.streaming.StreamingException;

//~import com.stc.connector.management.util.ObjectReference;


/**
 * The BatchRecord class represents what is shown to the user in the collab.
 * In addition to the default methods we have to implement from SimpleETDImpl
 * this is also where we expose the additional custom functions exposed to the
 * user (e.g. get(), put()) as well as the glue code to make the collab editor
 * work.
 * 
 * 
 * @author jim.fu@sun.com
 * @version 
 * @see BatchRecordConnector
 */
// This class is renamed from class BatchRecordETD.
public class BatchRecord {
    //private static final Messages mMessages =
    //        Messages.getMessages(BatchRecord.class);
    private static final Logger mLogger =
            Messages.getLogger(BatchRecord.class);
    private BatchRecordConfiguration mConfig;
    //~private ObjectReference mMonitor;
    /**
     * Defalt c'tor - create a new instance initializing everything
     * to default values.
     */
    public BatchRecord() {
        m_parser = null;     // created in initialize
        m_record = null;
        m_payload = null;
        m_workInProgress = false;
        m_insa = null;
        m_outsa = null;
        m_is = null;
        m_os = null;
    }
    
    /**
     *
     * @param p
     * @throws BatchException
     */
    public void initialize(Properties p) throws BatchException, BatchCFGException {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Enter BatchRecord.initialize");
        }
        try {
            mConfig = new BatchRecordConfiguration(p);
            //~this.mMonitor = (ObjectReference)p.get("monitor");
            m_parser = this.createParser();
        } catch (Exception e) {
            throw new BatchCFGException("Exception when initialize configuration for BatchRecord OTD, e=", e);
        }
    }
    
    
    /**
     * Resets the data content of an ETD.
     *
     * @return   <code>false</code> if the ETD doesn't have a meaningful
     *           implementation of <code>reset()</code>; so do a
     *           <code>new</code> of the ETD instead. Otherwise, return
     *           <code>true</code> if the reset will clear the data content
     *           of the ETD.
     */
    public boolean reset() throws BatchException {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Enter BatchRecord.reset");
        }
        
        // We're telling eGate that we know how to reset ourself. Consequently,
        // we're responsible for fully blowing everything away and starting
        // from scratch.
        m_parser = this.createParser();
        m_record = null;
        m_payload = null;
        m_workInProgress = false;
        releaseInput(false);
        releaseOutput(false);
        m_insa = null;
        m_outsa = null;
        
        return true;
    }
    
    /**
     *
     * @throws BatchException
     */
    public void terminate() throws BatchException {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Enter BatchRecord.terminate");
        }
        releaseInput(false);
        releaseOutput(false);
        m_insa = null;
        m_outsa = null;
    }
    
    /**
     *
     * @return
     * @throws BatchException
     */
    public boolean get() throws BatchException {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Enter BatchRecord.get");
        }
        String msg = null;
        
        try {
            if (getConfiguration().isCreateMode())
                throw new BatchException("The get() method should not be called on connection configured for create mode");
            
            if (null == m_is) {
                if (null != m_insa) {
                    m_payload = null;
                    try {
                        m_is = m_insa.requestInputStream();
                    } catch (StreamingException sex) {
                        msg = "Exception when requesting InputStream: " + sex.getMessage();
                        if (mLogger.isLoggable(Level.SEVERE)) {
                            mLogger.log(Level.SEVERE, msg);
                        }
                        throw new BatchException(msg, sex);
                    }
                } else {
                    m_is = new BatchByteArrayInputStream(m_payload);
                }
                m_workInProgress = true;
            }
            
            try {
                m_record = m_parser.get(m_is);
            } catch (Exception ex) {
                msg = "Exception thrown from parser.get: " + ex.getMessage();
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, msg);
                }
                m_workInProgress = false;
                releaseInput(false);
                throw new BatchException(msg, ex);
            }
            
            boolean bRet = m_record != null;
            if (bRet) {
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "BatchRecord.get: got a non null record from parser");
                }
            } else {
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "BatchRecord.get: did not get a record from parser");
                }
                // this is suppose to be end of the stream and it is a success
                // so release as success
                this.releaseInput(true);
            }
            
            return bRet;
        } catch (Exception e) {
            //~sendAlertAndThrowException(BatchRecordAlertCodes.BATCH_REC_EWAY_OPERATION_ERROR, new String[] {"get()", e.getMessage()}, e);
            throw new BatchException(e.getMessage(), e);
        }
        //~return true;
    }
    
    
    /**
     * ETD method: Called to append the record in the record attribute
     * to the output.
     *
     * @return boolean True if the parser.put() returns non-null,
     *         otherwise false.
     *
     * @throws BatchException, BatchException
     */
    public void put() throws BatchException {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Enter BatchRecord.put");
        }
        String msg = null;
        
        try {
            if (!getConfiguration().isCreateMode()) {
                throw new BatchException("The put() method should not be called on connection configured for parse mode");
            }
            
            if (null == m_os) {
                if (null != m_outsa) {
                    try {
                        m_os = m_outsa.requestOutputStream();
                    } catch (StreamingException sex) {
                        msg = "Exception when requesting OutputStream: " + sex.getMessage();
                        if (mLogger.isLoggable(Level.SEVERE)) {
                            mLogger.log(Level.SEVERE, msg);
                        }
                        throw new BatchException(msg, sex);
                    }
                } else {
                    m_os = new ByteArrayOutputStream();
                }
                m_workInProgress = true;
            }
            
            try {
                m_parser.put(m_os, m_record);
            } catch (Exception ex) {
                msg = "Exception thrown from parser.put: " + ex.getMessage();
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, msg);
                }
                m_workInProgress = false;
                releaseOutput(false);
                throw new BatchException(msg, ex);
            }
            
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "BatchRecord.put: Successfully stored a record");
            }
            return;
        } catch (Exception e) {
            //~sendAlertAndThrowException(BatchRecordAlertCodes.BATCH_REC_EWAY_OPERATION_ERROR, new String[] {"put()", e.getMessage()}, e);
            throw new BatchException(e.getMessage(), e);
        }
    }
    
    
    /**
     * ETD method: Called to finish the parse or create process
     *
     * @throws BatchException
     */
    public void finish() throws BatchException {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Enter BatchRecord.finish");
        }
        
        if (m_parser != null)
            try {
                m_parser.finish(m_os,m_is);
            } catch (Exception ex) {
                throw new BatchException(ex.toString());
            }
        
        if (!m_workInProgress)
            throw new BatchException("The finish() method should not be called when there is no work in progress");
        m_workInProgress = false;
        
        if (getConfiguration().isCreateMode()) {
            releaseOutput(true);
        } else {
            releaseInput(true);
        }
    }
    
    
    // ETD glue code method - collab editor will generate a method named
    // setOutputStreamAdapter if the user drags to the OutputStreamAdapter attrib.
    public void setOutputStreamAdapter(OutputStreamAdapter osa) throws BatchException {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Enter BatchRecord.setOutputStreamAdapter");
        }
        
        if (!getConfiguration().isCreateMode())
            throw new BatchException("The setOutputStreamAdapter() method should not be called on connection configured for parse mode");
        
        if (m_workInProgress)
            throw new BatchException("The setOutputStreamAdapter() method should not be called while there is still work in progress, call finished() first");
        
        m_outsa = osa;
        m_payload = null;
    }
    
    
    // ETD glue code method - collab editor will generate a method named
    // setInputStreamAdapter if the user drags to the InputStreamAdapter attrib.
    public void setInputStreamAdapter(InputStreamAdapter isa) throws BatchException {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Enter BatchRecord.setInputStream");
        }
        
        if (getConfiguration().isCreateMode())
            throw new BatchException("The setInputStreamAdapter() method should not be called on connection configured for create mode");
        
        if (m_workInProgress)
            throw new BatchException("The setInputStreamAdapter() method should not be called while there is still work in progress, call finished() first");
        
        m_insa = isa;
        m_payload = null;
    }
    
    
    // ETD glue code method - collab editor will generate a method named
    // getPayload if the user drags the payload attrib.
    public byte[] getPayload() throws BatchException {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Enter BatchRecord.getPayload");
        }
        
        if (!getConfiguration().isCreateMode())
            throw new BatchException("The getPayload() method should not be called on connection configured for parse mode");
        
        if (m_workInProgress)
            throw new BatchException("The getPayload() method should not be called while there is still work in progress, call finished() first");
        
        return m_payload;
    }
    
    
    // ETD glue code method - collab editor will generate a method named
    // setPayload if the user drags to the payload attrib.
    public void setPayload(byte[] data) throws BatchException {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Enter BatchRecord.setPayload");
        }
        if (getConfiguration().isCreateMode())
            throw new BatchException("The setPayload() method should not be called on connection configured for create mode");
        
        if (m_workInProgress)
            throw new BatchException("The setPayload() method should not be called while there is still work in progress, call finished() first");
        
        m_payload = data;
        m_insa = null;
    }
    
    
    // ETD glue code method - collab editor will generate a method named
    // getRecord if the user drags from the record attrib.
    public byte[] getRecord() throws BatchException {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Enter BatchRecord.getRecord");
        }
        return m_record;
    }
    
    
    // ETD glue code method - collab editor will generate a method named
    // setRecord if the user drags to the record attrib.
    public void setRecord(byte[] data) throws BatchException {
        if (mLogger.isLoggable(Level.FINE)) {
            if (data != null) {
                mLogger.log(Level.FINE, "BatchRecord.setRecord: non null record passed");
            } else {
                mLogger.log(Level.FINE, "BatchRecord.setRecord: null record passed");
            }
        }
        m_record = data;
    }
    
    
    // ETD glue code method to get the configuration.
    public BatchRecordConfiguration getConfiguration() {
        return this.mConfig;
    }
    
    public void setConfiguration(BatchRecordConfiguration cfg) {
        this.mConfig=cfg;
    }
    
    private BatchRecordParser m_parser = null;
    
    // This is the current "record".
    private byte[] m_record = null;
    
    // This is the current "payload".
    private byte[] m_payload = null;
    
    // This is the current InputStreamAdapter if any.
    // The ETD attribute is write-only.
    private InputStreamAdapter m_insa = null;
    
    // This is the current OutputStreamAdapter if any.
    // The ETD attribute is write-only.
    private OutputStreamAdapter m_outsa = null;
    
    // This is our working state. It is set to true on the first call to get()
    // or put(). It is set to false on initialization and the call to finish().
    private boolean m_workInProgress = false;
    
    // This is our InputStreamFindAdapter. It is used to provide the input
    // to the current parser get() method.
    private InputStream m_is = null;
    
    // This is our OutputStream. It is used to provide the output to the
    // current parser get() method.
    private OutputStream m_os = null;
    
    
    // Internal helper methods
    
    
    private void releaseInput(boolean success) throws BatchException {
        String msg = null;
        if (null != m_is) {
            if (null != m_insa) {
                m_is = null;
                try {
                    m_insa.releaseInputStream(success);
                } catch (StreamingException sex) {
                    msg = "Exception when releasing OutputStream: " + sex.getMessage();
                    if (mLogger.isLoggable(Level.SEVERE)) {
                        mLogger.log(Level.SEVERE, msg);
                    }
                    throw new BatchException(msg, sex);
                } finally {
                    m_insa = null;
                }
            } else {
                BatchByteArrayInputStream bas = (BatchByteArrayInputStream) m_is;
                m_is = null;
                try {
                    bas.close();
                } catch (IOException ioex) {
                    msg = "Exception when closing BatchByteArrayInputStream: " + ioex.getMessage();
                    if (mLogger.isLoggable(Level.SEVERE)) {
                        mLogger.log(Level.SEVERE, msg);
                    }
                    throw new BatchException(msg, ioex);
                }
            }
        }
    }
    
    private void releaseOutput(boolean success) throws BatchException {
        String msg = null;
        if (null != m_os) {
            if (null != m_outsa) {
                m_os = null;
                try {
                    m_outsa.releaseOutputStream(success);
                } catch (StreamingException sex) {
                    msg = "Exception when releasing OutputStream: " + sex.getMessage();
                    if (mLogger.isLoggable(Level.SEVERE)) {
                        mLogger.log(Level.SEVERE, msg);
                    }
                    throw new BatchException(msg, sex);
                } finally {
                    m_outsa = null;
                }
            } else {
                ByteArrayOutputStream bas = (ByteArrayOutputStream)m_os;
                m_os = null;
                try {
                    m_payload = null;
                    bas.close();
                    if (success) {
                        m_payload = bas.toByteArray();
                    }
                } catch (IOException ioex) {
                    msg = "Exception when closing ByteArrayOutputStream: " + ioex.getMessage();
                    if (mLogger.isLoggable(Level.SEVERE)) {
                        mLogger.log(Level.SEVERE, msg);
                    }
                    throw new BatchException(msg, ioex);
                }
            }
        }
    }
    
    private BatchRecordParser createParser() throws BatchException {
        BatchRecordParser parser = null;
        try {
            String sParserClassName = getConfiguration().getParserClassName();
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "BatchRecordConnector.createParser: attempting to create " + sParserClassName);
            }
            if ( sParserClassName == null || sParserClassName.trim().length() == 0 )
                throw new Exception("BatchRecord reset(): parser class not available.");
            parser = BatchRecordParserFactory.createParser(sParserClassName);
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "BatchRecordConnector.createParser: calling parser.initialize");
            }
            parser.initialize(this.getConfiguration());
        } catch (Exception ex) {
            String msg = "Exception thrown from BatchRecordParserFactory.createParser:" + ex.getMessage();
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new BatchException(msg, ex);
        }
        return parser;
    }
    
    //~public ObjectReference getMonitor() {
    //~    return this.mMonitor;
    //~}
    
    //~private void sendAlertAndThrowException(String aKey, String[] aParams, Exception e) throws BatchException {
    //~    String msg = BatchAlertUtil.getMessage(aKey, aParams);
    //~    BatchAlertUtil.sendAlert(
    //~            this.getMonitor(),
    //~            aKey,
    //~            aKey,
    //~            aParams,
    //~            NotificationEvent.SEVERITY_TYPE_MAJOR);
    //~    if ( e != null )
    //~        throw new BatchException(msg, e);
    //~    else
    //~        throw new BatchException(msg);
    //~}
}
