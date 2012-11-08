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
 * @(#)Profile.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.project.scrub;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import javax.xml.xpath.XPathConstants;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.common.xml.XmlUtil;
import com.sun.jbi.component.toolkit.project.util.I18n;
import com.sun.jbi.component.toolkit.project.util.ProjectException;
import com.sun.jbi.component.toolkit.project.util.XPathElement;

/**
 * 
 * @author Kevan Simpson
 */
public class Profile extends XPathElement {//XmlObject<ScrubExpr> {
    public enum ProjType { 
        ojc("OJC Core"), contrib("OJC Contribution"), external("Independent Project");
        
        private String mDesc;
        
        private ProjType(String desc) {
            mDesc = desc;
        }
        
        public String getDescription() {
            return mDesc;
        }
        
        public static ProjType descriptionOf(String desc) {
            if (!Util.isEmpty(desc)) {
                for (ProjType pt : values()) {
                    if (desc.equals(pt.getDescription())) {
                        return pt;
                    }
                }
            }
            
            return null;
        }
    }

    private String mName;
    private File mProjRoot;
    private Properties mTokens;
    private List<Scrubber> mScrubbers;
    private Map<String, Element> mUnitDefs;

    public Profile(Element elem, File file, String name, File projRoot, Properties props) {
        super(file, elem);
        init(name, projRoot, props);
//        super(elem, file, ScrubExpr.values(), name, projRoot, props);
    }

    public String getProperty(String key) {
        return mTokens.getProperty(key, "@MISSING@");
    }
    
    public String getName() {
        return mName;
    }
    
    public Element lookupUnitDef(Element unit) {
        String ref = unit.getAttribute("ref");
        return (Util.isEmpty(ref)) ? unit : mUnitDefs.get(ref);
    }
    
    public Iterator<Scrubber> scrubbers() {
        return mScrubbers.iterator();
    }
    
    protected void init(String name, File projRoot, Properties props) {
        mName = name;
        mProjRoot = projRoot;
        mTokens = props;
        mScrubbers = new ArrayList<Scrubber>();
        // load unit-defs
        mUnitDefs = new HashMap<String, Element>();
        NodeList defs = (NodeList) evaluate(
                ((Element) getElement().getParentNode()),
                ScrubExpr.unit_defs.getXPath(), 
                XPathConstants.NODESET);
        for (int i = 0, n = defs.getLength(); i < n; i++) {
            Element unit = (Element) defs.item(i);
            mUnitDefs.put(unit.getAttribute("name"), unit);
        }

        // load file scrub defs
        NodeList files = getNodeSet(ScrubExpr.files.getXPath());
        List<ProjectException> errs = new ArrayList<ProjectException>();
        if (files == null) {    // XXX
            errs.add(new ProjectException(I18n.loc(
                    "Profile {0} has 0 files configured!", mName)));
        }
        else {
            for (int i = 0, n = files.getLength(); i < n; i++) {
                Element elemFile = (Element) files.item(i);
                File file = new File(mProjRoot, elemFile.getAttribute("path"));
                if (file.exists()) {
                    // each file is scrubbed atomically, record errors not throw
                    try {
                        // need element to help Regex find the correct node
                        Document doc = XmlUtil.readXml(file);
                        Scrubber scrubber = 
                            new Scrubber(elemFile,
                                    getFile(),
                                    file,
                                    doc.getDocumentElement(),
                                    this);
                        mScrubbers.add(scrubber);
                    }
                    catch (Exception e) {
                        errs.add(new ProjectException(I18n.loc( // XXX
                                "Scrub failed for {0}: {1}", 
                                file.getAbsolutePath(), e.getMessage()), e));
                    }
                }
            }
        }

    }
}
