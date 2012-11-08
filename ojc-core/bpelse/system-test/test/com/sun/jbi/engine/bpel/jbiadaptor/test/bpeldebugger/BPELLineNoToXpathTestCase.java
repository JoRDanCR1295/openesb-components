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
 * @(#)BPELLineNoToXpathTestCase.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.bpeldebugger;

import java.io.File;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import javax.wsdl.Definition;

import junit.framework.TestCase;

import org.xml.sax.EntityResolver;

import com.sun.bpel.model.Activity;
import com.sun.bpel.model.BPELDocument;
import com.sun.bpel.model.BPELDocumentParseFactory;
import com.sun.bpel.model.BPELParseContext;
import com.sun.bpel.model.BPELProcess;
import com.sun.bpel.model.DefaultWSDLResolverFactory;
import com.sun.bpel.model.MultipleActivityHolder;
import com.sun.bpel.model.util.ParsingCaches;
import com.sun.bpel.model.visitor.IWSDLResolver;
import com.sun.bpel.xml.wsdl.WSDLDocument;
import com.sun.bpel.xml.xsd.XMLSchema;
//import com.sun.jbi.engine.bpel.core.bpel.model.parser.impl.ParseContextImpl;
import com.sun.wsdl4j.ext.DeferredActionRegistry;
import com.sun.wsdl4j.ext.WSDL4JExt;

public class BPELLineNoToXpathTestCase extends TestCase {

   static class ActivityLineXpath implements Comparable {
        public String xpath;
        public String activityName;
        public int lineNo;

        public int compareTo(Object o) {
            // TODO Auto-generated method stub
            return lineNo - ((ActivityLineXpath) o).lineNo;
        }

        @Override
        public String toString() {
            // TODO Auto-generated method stub
            return "Activity :" + activityName +  " Xpath :" + xpath + " lineNo:" + lineNo;
        }


    }



    private static final String testPropertyFile = "bpdebugger/test.properties";
    private static final String BPELFILE = "simpleFlow" + File.separator + "simpleFlow.bpel";
    private File baseDir;

    //HashMap<lineNo, ActivityLineXpath>
    private LinkedHashMap activityLineXpathMap = new LinkedHashMap ();



    public BPELLineNoToXpathTestCase () throws Exception {
        URL url = getClass().getResource(testPropertyFile);
        File testProperties =  new File(url.toURI());
        baseDir = testProperties.getParentFile();
   }


    //This tests finding a correct breakable activity based on a lineNo passed in
    public void testLineNoToBreakPoint () throws Exception {
        //Test: simpleFlow.bpel

        setUPMap ("simpleFlow" + File.separator + "simpleFlow.bpel");

        ActivityLineXpath act = getActivity(8, activityLineXpathMap);
        assertEquals(13, act.lineNo);
        System.out.println(act);

        act = getActivity(23, activityLineXpathMap);
        assertEquals(23, act.lineNo);
        System.out.println(act);

        act = getActivity(37, activityLineXpathMap);
        assertEquals(35, act.lineNo);
        System.out.println(act);

        //Test: Invoke1parent.bpel
        setUPMap ("correlation_invoke1_parent" + File.separator + "Invoke1parent.bpel");

        act = getActivity(66, activityLineXpathMap);
        assertEquals(71, act.lineNo);
        System.out.println(act);

        act = getActivity(90, activityLineXpathMap);
        assertEquals(92, act.lineNo);
        System.out.println(act);


    }

    private void setUPMap(String fileName) throws Exception {
        // TODO Auto-generated method stub
        BPELProcess bpelPrc = parseBPEL(baseDir.getAbsolutePath() + File.separator + fileName);
        Set allActivities = getAllBPELActivities(bpelPrc);
        activityLineXpathMap.clear();
        for (Iterator it = allActivities.iterator(); it.hasNext();) {
            ActivityLineXpath activityPath = (ActivityLineXpath) it.next();
            activityLineXpathMap.put(new Integer(activityPath.lineNo), activityPath);
        }

    }



    //Pass in any lineNo, this method goes through all valid lineNos and :
    //picks the valid breakpoint line based on:
    // 1. The minimum value that >= lineNo
    // 2. if the lineNo > the max of valid lineNo, the max of valid lineNo
    // 3. if the lineNo < the min of valid lineNo, the min of valid lineNo
    //
    // actMap is already sorted
    private static ActivityLineXpath getActivity (int lineNo, HashMap actMap) {
        Set activityLineSet = actMap.keySet();
        List actLineLst = new ArrayList();
        actLineLst.addAll(activityLineSet);
        Integer lastLine = (Integer) actLineLst.get(actLineLst.size()- 1);
        Integer firstLine = (Integer) actLineLst.get(0);

        if (lineNo >= lastLine.intValue()) {
            return (ActivityLineXpath) actMap.get(lastLine);
        } else if (lineNo <= firstLine) {
            return (ActivityLineXpath) actMap.get(firstLine);
        }
        else {
            for (int i = 0; i < actLineLst.size(); i++) {
                Integer actLine = (Integer) actLineLst.get(i);
                if (lineNo <= actLine.intValue()) {
                    return (ActivityLineXpath) actMap.get(actLine);
                }
            }
        }
        return null;

    }


    private static BPELProcess parseBPEL (String bpelFileName) throws Exception {
        HashSet ret = new HashSet();
        URL url = null;
        InputStream is = null;
        InputStreamReader reader = null;
        EntityResolver resolver = null;

        url = new URL("file", null, bpelFileName);
        is = url.openStream();
        reader = new InputStreamReader(is);

        BPELParseContext parseContext = new BPELParseContext.DefaultParseContext();
        String bpelFileURI = new File (bpelFileName).toURI().toString();
        IWSDLResolver wsdlResolver = DefaultWSDLResolverFactory.getInstance().newWSDLResolver(
                bpelFileURI, parseContext);
        parseContext.setWSDLResolver(wsdlResolver);

        ParsingCaches caches = new ParsingCaches();
        parseContext.setCaches(caches);
        DeferredActionRegistry registry = new DeferredActionRegistry();
        parseContext.setDeferredActionRegistry(registry);
        BPELDocument bpelDoc = BPELDocumentParseFactory.getInstance().load(reader, parseContext);
        WSDL4JExt.applySingleSchemaTypeLoader(registry, parseContext.getBaseURIResolver());
        bpelDoc.setBaseURI(bpelFileName);

        BPELProcess bProc = bpelDoc.getDocumentProcess();
        return bProc;
    }

    private static ActivityLineXpath makeActivityLineXpath (Activity act) {
        ActivityLineXpath actLineXpath = new ActivityLineXpath ();
        actLineXpath.lineNo = act.getLocator().getLineNumber();
        actLineXpath.activityName = act.getName();
        actLineXpath.xpath = act.getXPath();
        return actLineXpath;
    }

    //Returns a sorted Set for activities, ordered by their lineno.
    private static Set getAllBPELActivities (BPELProcess bpelProcess) throws Exception {
      TreeSet allActSet = new TreeSet ();

      Activity act = bpelProcess.getActivity();
      getAllBPELActivities(act, allActSet);
      return allActSet;
    }

    private static void getAllBPELActivities (Activity act, Set allActs) throws Exception {
        allActs.add(makeActivityLineXpath (act));
        if (act instanceof MultipleActivityHolder) {
            MultipleActivityHolder holder = (MultipleActivityHolder)act;
            Collection acts = holder.getActivities();
            for (Iterator it = acts.iterator(); it.hasNext();) {
                Activity childact = (Activity) it.next();
                allActs.add(makeActivityLineXpath (childact));
                getAllBPELActivities(childact, allActs);
            }
        }
    }


}
