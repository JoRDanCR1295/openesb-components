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
 * @(#)$Id: KeywordsImpl.java,v 1.1 2010/02/04 02:53:27 fkieviet Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.workflow.model.impl;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.xml.namespace.QName;

import org.apache.xmlbeans.XmlObject;

import com.sun.jbi.workflow.model.Keyword;
import com.sun.jbi.workflow.model.Keywords;
import com.sun.jbi.workflow.model.LocalNotification;
import com.sun.jbi.workflow.model.ModelElement;
import com.sun.jbi.workflow.model.Task;
import com.sun.jbi.workflow.model.XPathInfo;
import com.sun.jbi.workflow.model.xmlbeans.TAction;
import com.sun.jbi.workflow.model.xmlbeans.TChangeVariables;
import com.sun.jbi.workflow.model.xmlbeans.TExpression;
import com.sun.jbi.workflow.model.xmlbeans.TKeywords;
import com.sun.jbi.workflow.model.xmlbeans.TLocalNotification;

public class KeywordsImpl extends ModelElementImpl  implements Keywords {
    private TKeywords mKeywordsType = null;
    private List<Keyword> mKeywords = new ArrayList<Keyword> ();
    
    public KeywordsImpl(TKeywords keywords, ModelElement parent) {
        super(keywords, parent);
        this.mKeywordsType = keywords;
        init();
    }    

    public List<Keyword> getKeywords() {
        // TODO Auto-generated method stub
        return mKeywords;
    }
    
    private void init() {
        List<TExpression> keywordList = this.mKeywordsType.getKeywordList();
        if (keywordList != null && keywordList.size() > 0) {
            Iterator<TExpression> it = keywordList.iterator();
            while (it.hasNext()) {
                TExpression ln = it.next();
                Keyword keyword = new KeywordImpl(
                        ln, this);
                this.mKeywords.add(keyword);
            }
        }        
    }
}
