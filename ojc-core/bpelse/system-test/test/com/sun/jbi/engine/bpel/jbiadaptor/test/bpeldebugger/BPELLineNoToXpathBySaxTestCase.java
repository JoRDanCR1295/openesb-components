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
 * @(#)BPELLineNoToXpathBySaxTestCase.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.bpeldebugger;

import java.io.File;
import java.io.FileReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Set;

import junit.framework.TestCase;

import org.xml.sax.InputSource;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.XMLReaderFactory;
//import com.sun.jbi.engine.bpel.core.bpel.model.parser.impl.ParseContextImpl;

import com.sun.org.apache.xml.internal.utils.XMLReaderManager;

public class BPELLineNoToXpathBySaxTestCase extends TestCase {
    private static final String testPropertyFile = "bpdebugger/test.properties";
    private static final String BPELFILE = "simpleFlow" + File.separator + "simpleFlow.bpel";
    private File baseDir;

    //HashMap<lineNo, ActivityLineXpath>
    private LinkedHashMap activityLineXpathMap = new LinkedHashMap ();



    public BPELLineNoToXpathBySaxTestCase () throws Exception {
        URL url = getClass().getResource(testPropertyFile);
        File testProperties =  new File(url.toURI());
        baseDir = testProperties.getParentFile();
   }


    //This tests finding a correct breakable activity based on a lineNo passed in
    public void testLineNoToBreakPoint () throws Exception {
        //Test: simpleFlow.bpel

        BPELActivityFinderSaxHandler saxHandler = setUPMap ("simpleFlow" + File.separator + "simpleFlow.bpel");

        BPELNode bpelnode = getActivity(8, activityLineXpathMap, saxHandler.getFirstActivity(), saxHandler.getLastActivity());
        assertEquals(13, bpelnode.getLineNo());
        System.out.println(bpelnode);

        bpelnode = getActivity(23, activityLineXpathMap, saxHandler.getFirstActivity(), saxHandler.getLastActivity());
        assertEquals(23, bpelnode.getLineNo());
        System.out.println(bpelnode);

        bpelnode = getActivity(37, activityLineXpathMap, saxHandler.getFirstActivity(), saxHandler.getLastActivity());
        assertEquals(35, bpelnode.getLineNo());
        System.out.println(bpelnode);

        //Test: Invoke1parent.bpel
        saxHandler = setUPMap ("correlation_invoke1_parent" + File.separator + "Invoke1parent.bpel");

        bpelnode = getActivity(66, activityLineXpathMap, saxHandler.getFirstActivity(), saxHandler.getLastActivity());
        assertEquals(71, bpelnode.getLineNo());
        System.out.println(bpelnode);

        bpelnode = getActivity(90, activityLineXpathMap, saxHandler.getFirstActivity(), saxHandler.getLastActivity());
        assertEquals(92, bpelnode.getLineNo());
        System.out.println(bpelnode);

        //Test: AS_ContExecAfterFaultedScope1.bpel
        saxHandler = setUPMap ("faulthandling_child" + File.separator + "AS_ContExecAfterFaultedScope1.bpel");
        bpelnode = getActivity(47, activityLineXpathMap, saxHandler.getFirstActivity(), saxHandler.getLastActivity());
        assertEquals(45, bpelnode.getLineNo());
        System.out.println(bpelnode);




    }

    private BPELActivityFinderSaxHandler setUPMap(String fileName) throws Exception {
        // TODO Auto-generated method stub
        BPELActivityFinderSaxHandler saxHandler = new BPELActivityFinderSaxHandler();
        XMLReader xr = null;
        try {
            xr = XMLReaderManager.getInstance().getXMLReader();
            xr.setFeature("http://xml.org/sax/features/namespaces", true);
            xr.setContentHandler(saxHandler);
            xr.setErrorHandler(saxHandler);
            xr.parse(new InputSource(new FileReader(baseDir.getAbsolutePath()
                    + File.separator + fileName)));

            List allNodes = saxHandler.getAllNodes();
            activityLineXpathMap.clear();
            for (Iterator it = allNodes.iterator(); it.hasNext();) {
                BPELNode bpelNode = (BPELNode) it.next();
                activityLineXpathMap.put(new Integer(bpelNode.getLineNo()),
                        bpelNode);
            }
        } catch (Exception e) {
            throw e;
        } finally {
            if (xr != null) {
                XMLReaderManager.getInstance().releaseXMLReader(xr);
            }
        }
        return saxHandler;

    }



    // Pass in any lineNo, this method goes through all valid lineNos and :
    //picks the valid breakpoint line based on:
    // 1. The minimum value that >= lineNo
    // 2. if the lineNo > the max of valid lineNo, the max of valid lineNo
    // 3. if the lineNo < the min of valid lineNo, the min of valid lineNo
    //
    // actMap is already sorted
    private static BPELNode getActivity (int lineNo, HashMap actMap, BPELNode firstAct, BPELNode lastAct) {
        Set activityLineSet = actMap.keySet();
        List actLineLst = new ArrayList();
        actLineLst.addAll(activityLineSet);
        int lastLine = lastAct.getLineNo();
        int firstLine = firstAct.getLineNo();

        if (lineNo >= lastLine) {
            return lastAct;
        } else if (lineNo <= firstLine) {
            return firstAct;
        }
        else {
            for (int i = 0; i < actLineLst.size(); i++) {
                Integer actLine = (Integer) actLineLst.get(i);
                if (lineNo <= actLine.intValue()) {
                    return getActNode((BPELNode) actMap.get(actLine));
                }
            }
        }
        return null;

    }

    private static BPELNode getActNode (BPELNode bpelNode) {
        if (bpelNode == null)
            return null;
        if (bpelNode.isActivity()) {
            return bpelNode;
        }else {
            return getActNode(bpelNode.getParent());
        }

    }

}
