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
 * @(#)BPELActivityFinderSaxHandler.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.bpeldebugger;

import java.util.ArrayList;
import java.util.List;

import org.xml.sax.Attributes;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import com.sun.jbi.engine.bpel.jbiadaptor.test.bpeldebugger.BPELNode.BPELNodeType;

/**
 * A Handler that construct BPEL node tree and finds all the Activities 
 * @author Sun Microsystems
 *
 */
public class BPELActivityFinderSaxHandler extends DefaultHandler {
    
//    private List mAllActivities = new ArrayList ();
    private List mAllNodes = new ArrayList ();
    private BPELNode mCurrentNode;
    private Locator mLocator;
    private String mTargetNS;
    private BPELNode mFirstAct;
    private BPELNode mLastAct;
    
    
//    public List getAllActivities () {
//        return mAllActivities;
//    }
    
    public List getAllNodes () {
        return mAllNodes;
    }

    @Override
    public void endElement(String uri, String localName, String qName) throws SAXException {
        // TODO Auto-generated method stub
        mCurrentNode = mCurrentNode.getParent ();
    }

    @Override
    public void setDocumentLocator(Locator locator) {
        // TODO Auto-generated method stub
        mLocator = locator;
    }

    @Override
    public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
        // TODO Auto-generated method stub
        if (mCurrentNode == null) {
            for (int i=0; i< attributes.getLength(); i++) {
                if (attributes.getLocalName(i).equals ("targetNamespace")) {
                    mTargetNS = attributes.getValue(i);
                }
            }            
        }
        BPELNodeType nodeType = BPELNode.getNodeType(localName);
        BPELNode bpelNode = new BPELNode (localName, mLocator.getLineNumber(), mTargetNS, nodeType, mCurrentNode);
        if (mCurrentNode != null) {
            mCurrentNode.addChild(bpelNode);
        }
//        if (bpelNode.isActivity()) {
//            mAllActivities.add(bpelNode);
//        }
        mAllNodes.add(bpelNode);
        if (bpelNode.isActivity() && mFirstAct == null) {
            mFirstAct = bpelNode;
        } else if (bpelNode.isActivity()) {
            mLastAct = bpelNode;
        }
        mCurrentNode = bpelNode;         
    }
    
    public BPELNode getFirstActivity () {
        return mFirstAct;
    }
    
    public BPELNode getLastActivity () {
        return mLastAct;
    }
    

}
