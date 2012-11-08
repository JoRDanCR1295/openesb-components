/*
 * Copyright 2005 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */
package com.sun.jbi.engine.scriptse;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.component.endpoint.impl.AbstractEndpoint;
import com.sun.jbi.engine.scriptse.process.ScriptInfoVO;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.namespace.QName;
import javax.xml.transform.Templates;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerFactory;


/**
 * Script SE {@link Endpoint} implementation.
 *
 * @author Prashanth B.R
 */
public class ScriptseEndpoint extends AbstractEndpoint {
    private static Logger msLogger = Logger.getLogger(ScriptseEndpoint.class.getName());
    private static TransformerFactory mTransformerFactory = TransformerFactory.newInstance();
    private boolean mTransformJBI = false;
    private Templates mTemplates = null;
    private QName mMessageType = null;
    private QName mOperation = null;
    private ScriptseEndpoint mInvoke = null;
    private EntryType mEntryType = null;
    private String mAddXSDJarFilePath = null;
    private String mSUInstallPath = null;
    private ScriptInfoVO mScrInfo = null;
    private String serviceUnitName;

    /**
     * Creates a new ScriptseEndpoint object.
     *
     * @param info DOCUMENT ME!
     */
    public ScriptseEndpoint(EndpointInfo info) {
        super(info);
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public ScriptInfoVO getMScrInfo() {
        return mScrInfo;
    }

    /**
     * DOCUMENT ME!
     *
     * @param mScrInfo DOCUMENT ME!
     */
    public void setMScrInfo(ScriptInfoVO mScrInfo) {
        this.mScrInfo = mScrInfo;
    }

    /**
     * DOCUMENT ME!
     *
     * @param mSUInstallPath DOCUMENT ME!
     */
    public void setMSUInstallPath(String mSUInstallPath) {
        this.mSUInstallPath = mSUInstallPath;
    }

    /**
     * DOCUMENT ME!
     *
     * @param serviceUnitName DOCUMENT ME!
     */
    public void setServiceUnitName(String serviceUnitName) {
        this.serviceUnitName = serviceUnitName;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getServiceUnitName() {
        return this.serviceUnitName;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getMSUInstallPath() {
        return mSUInstallPath;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getMAddXSDJarFilePath() {
        return mAddXSDJarFilePath;
    }

    /**
     * DOCUMENT ME!
     *
     * @param mAddXSDJarFilePath DOCUMENT ME!
     */
    public void setMAddXSDJarFilePath(String mAddXSDJarFilePath) {
        this.mAddXSDJarFilePath = mAddXSDJarFilePath;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public EntryType getEntryType() {
        return mEntryType;
    }

    /**
     * DOCUMENT ME!
     *
     * @param type DOCUMENT ME!
     */
    protected void setEntryType(EntryType type) {
        mEntryType = type;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public ScriptseEndpoint getInvoke() {
        return mInvoke;
    }

    /**
     * DOCUMENT ME!
     *
     * @param invoke DOCUMENT ME!
     */
    protected void setInvoke(ScriptseEndpoint invoke) {
        mInvoke = invoke;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean isTransformJBI() {
        return mTransformJBI;
    }

    /**
     * DOCUMENT ME!
     *
     * @param isTransformJBI DOCUMENT ME!
     */
    public void setTransformJBI(boolean isTransformJBI) {
        mTransformJBI = isTransformJBI;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public QName getMessageType() {
        return mMessageType;
    }

    /**
     * DOCUMENT ME!
     *
     * @param messageType DOCUMENT ME!
     */
    public void setMessageType(QName messageType) {
        mMessageType = messageType;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public QName getOperation() {
        return mOperation;
    }

    /**
     * DOCUMENT ME!
     *
     * @param opName DOCUMENT ME!
     */
    public void setOperation(QName opName) {
        mOperation = opName;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     *
     * @throws TransformerConfigurationException DOCUMENT ME!
     */
    public Transformer getTransformer() throws TransformerConfigurationException {
        if (mTemplates == null) {
            synchronized (mTransformerFactory) {
                return mTransformerFactory.newTransformer();
            }
        }

        return mTemplates.newTransformer();
    }

    /**
     * DOCUMENT ME!
     *
     * @param templates DOCUMENT ME!
     */
    protected void setTemplates(Templates templates) {
        mTemplates = templates;
    }

    /**
     * DOCUMENT ME!
     */
    public void print() {
        try {
            msLogger.log(Level.INFO, "mMessageType ::" + mMessageType.toString());
            msLogger.log(Level.INFO, "mOperation :: " + mOperation.toString());
            msLogger.log(Level.INFO, "mAddXSDJarFilePath:: " + mAddXSDJarFilePath);
            msLogger.log(Level.INFO, "mSUInstallPath :: " + mSUInstallPath);
            msLogger.log(Level.INFO, "mTransformJBI :: " + mTransformJBI);
        } catch (Exception e) {
            msLogger.log(Level.INFO, "Exception while printing ScripteSEEndPoint:: " + e);
        }
    }
    public enum EntryType {REQUEST_REPLY,
        FILTER_ONE_WAY,
        FILTER_REQUEST_REPLY;
    }
} //class ScriptseEndpoint  ends.
