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
 * @(#)TestBeanWithNode.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.axes;

import org.apache.commons.jxpath.JXPathTestCase;
import org.apache.commons.jxpath.TestBean;
import org.apache.commons.jxpath.xml.DocumentContainer;
import org.w3c.dom.Document;

/**
 * Test bean for mixed model JUnit tests.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class TestBeanWithNode extends TestBean {
    private Object node;
    private Object object;

    public Object getVendor() {
        return node;
    }

    public Object[] getVendors() {
        return new Object[] { node };
    }

    public void setVendor(Object node) {
        this.node = node;
    }

    public Object getObject() {
        return object;
    }

    public void setObject(Object object) {
        this.object = object;
    }

    public static TestBeanWithNode createTestBeanWithDOM() {
        DocumentContainer docCtr =
            new DocumentContainer(
                JXPathTestCase.class.getResource("Vendor.xml"));
        Document doc = (Document) docCtr.getValue();
        TestBeanWithNode tbwdom = new TestBeanWithNode();
        tbwdom.setVendor(doc.getDocumentElement());
        tbwdom.setObject(docCtr);
        return tbwdom;
    }

}
