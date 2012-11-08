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
 * @(#)ScreenScrapingseEndpoint.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package org.glassfish.openesb.engine.screenscrapingse.jbiadapter;

import com.sun.jbi.common.descriptor.EndpointInfo;
import org.glassfish.openesb.engine.screenscrapingse.process.ScriptInfoVO;
import com.sun.jbi.component.toolkit.endpoint.impl.AbstractEndpoint;


import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.namespace.QName;
import javax.xml.transform.Templates;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerFactory;

import com.sun.jbi.common.qos.redelivery.RedeliveryConfig;


public class ScreenScrapingseEndpoint extends AbstractEndpoint<ScriptInfoVO> {

    private static TransformerFactory mTransformerFactory = TransformerFactory.newInstance();
    private boolean mTransformJBI = false;
    private Templates mTemplates = null;
    private QName mMessageType = null;
    private QName mOperation = null;
    private ScreenScrapingseEndpoint mInvoke = null;
    private EntryType mEntryType = null;
    private String mAddXSDJarFilePath = null;
    private String mSUInstallPath = null;
    private ScriptInfoVO mScrInfo = null;
    private String serviceUnitName;

    protected RedeliveryConfig mRedeliveryConfig;

    /**
     * Creates a new ScreenScrapingseEndpoint object.
     *
     * @param info DOCUMENT ME!
     */
    public ScreenScrapingseEndpoint(EndpointInfo info) {
        super(info);
    }
    
    public RedeliveryConfig getRedeliveryConfiguration()
    {
        return mRedeliveryConfig;
    }
    
    public void setRedeliveryConfiguration(RedeliveryConfig redeliveryConfig)
    {
        mRedeliveryConfig = redeliveryConfig;
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
    public ScreenScrapingseEndpoint getInvoke() {
        return mInvoke;
    }

    /**
     * DOCUMENT ME!
     *
     * @param invoke DOCUMENT ME!
     */
    protected void setInvoke(ScreenScrapingseEndpoint invoke) {
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
    public Transformer getTransformer()
            throws TransformerConfigurationException {
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

   public enum EntryType {

        REQUEST_REPLY,
        FILTER_ONE_WAY,
        FILTER_REQUEST_REPLY;
    }
} //class ScreenScrapingseEndpoint  ends.
