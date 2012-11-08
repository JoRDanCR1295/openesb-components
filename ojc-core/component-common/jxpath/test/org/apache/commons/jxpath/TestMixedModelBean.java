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
 * @(#)TestMixedModelBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.jxpath.xml.DocumentContainer;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * Mixed model test bean: Java, collections, map, DOM, Container.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class TestMixedModelBean {
    private String string;
    private TestBean bean;
    private Container container;
    private Document document;
    private Element element;

    private Map map;

    private List list;
    private int[][] matrix;

    public TestMixedModelBean() {
        string = "string";
        bean = new TestBean();
        map = new HashMap();
        list = new ArrayList();

        container = new DocumentContainer(getClass().getResource("Vendor.xml"));
        document = (Document) container.getValue();
        element = document.getDocumentElement();

        map.put("string", string);
        map.put("bean", bean);
        map.put("map", map);
        map.put("list", list);
        map.put("document", document);
        map.put("element", element);
        map.put("container", container);

        list.add(string);
        list.add(bean);
        list.add(map);
        list.add(new ArrayList(Collections.singletonList("string2")));
        list.add(document);
        list.add(element);
        list.add(container);

        matrix = new int[1][];
        matrix[0] = new int[1];
        matrix[0][0] = 3;
    }

    public String getString() {
        return string;
    }

    public TestBean getBean() {
        return bean;
    }

    public Map getMap() {
        return map;
    }

    public List getList() {
        return list;
    }

    public Document getDocument() {
        return document;
    }

    public Element getElement() {
        return element;
    }

    public Container getContainer() {
        return container;
    }

    public int[][] getMatrix() {
        return matrix;
    }

    public void setMatrix(int[][] matrix) {
        this.matrix = matrix;
    }
}
