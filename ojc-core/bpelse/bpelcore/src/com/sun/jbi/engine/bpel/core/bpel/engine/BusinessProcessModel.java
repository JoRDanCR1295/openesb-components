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
 * @(#)BusinessProcessModel.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.engine;

import java.util.List;

import javax.xml.namespace.QName;

import com.sun.bpel.model.PartnerLink;
import com.sun.bpel.model.meta.RBPELProcess;


/**
 * BP Model
 *
 * @author Sun Microsystems
 */
public class BusinessProcessModel {
    private String mOperation;
    private PartnerLink mPartnerLink;
    private QName mPort;
    private String mId;
    private RBPELProcess mModel;
    private int mType;
    private String mMessageExchange;
    private List mCorrelationSets;
    private boolean mHasCorrelationSet;

    /**
     * Creates a new instance of BusinessProcessModel
     *
     * @param partnerLink partner link
     * @param portType portType
     * @param oper operation
     * @param model runtime bpel process
     * @param type type
     * @param messageExchange message exchange
     * @param corrSet list of correlation sets
     */
    public BusinessProcessModel(PartnerLink partnerLink, QName portType,
        String oper, RBPELProcess model, int type, String messageExchange,
        List corrSet) {
        mOperation = oper;
        mPartnerLink = partnerLink;
        mPort = portType;

        // mId = model.getBPELId();
        // TODO will the following baseURI work for the above old model bpelID.
        mId = model.getOwnerDocument().getBaseURI();
        mModel = model;
        mType = type;
        mMessageExchange = messageExchange;
        mCorrelationSets = corrSet;

        if (corrSet == null) {
            mHasCorrelationSet = false;
        }
    }

    /**
     * gets operation
     *
     * @return String operation
     */
    public String getOperation() {
        assert mOperation != null;

        return mOperation;
    }

    /**
     * gets Port
     *
     * @return QName port
     */
    public QName getPort() {
        return mPort;
    }

    /**
     * gets partner link
     *
     * @return PartnerLink partner link
     */
    public PartnerLink getpartnerLink() {
        assert mPartnerLink != null;

        return mPartnerLink;
    }

    /**
     * gets model
     *
     * @return RBPELProcess runtime bpel process
     */
    public RBPELProcess getModel() {
        assert mModel != null;

        return mModel;
    }

    /**
     * gets ID
     *
     * @return String ID
     */
    public String getId() {
        return mId;
    }

    /**
     * ges Type
     *
     * @return int type
     */
    public int getType() {
        assert (mType >= 0) && (mType <= 3);

        return mType;
    }

    /**
     * gets correlation sets
     *
     * @return List correlation sets
     */
    public List getCorrelationSets() {
        return mCorrelationSets;
    }

    /**
     * check if model contains correlationset
     *
     * @return boolean: if the model contains correlationset, returns true; otherwise, returns
     *         false
     */
    public boolean hasCorrelationSet() {
        return mHasCorrelationSet;
    }

    /**
     * gets message exchange
     *
     * @return Returns the mMessageExchange.
     */
    public String getMessageExchange() {
        return mMessageExchange;
    }
}
