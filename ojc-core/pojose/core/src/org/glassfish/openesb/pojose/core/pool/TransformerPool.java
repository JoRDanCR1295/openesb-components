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
 * @(#)TransformerPool.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */


package org.glassfish.openesb.pojose.core.pool;

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerFactory;
import org.glassfish.openesb.pojose.core.util.Util;

/**
 *
 * @author gpatil
 */
public class TransformerPool extends AbstractPool{
    private static volatile TransformerPool instance = null;
    
    private TransformerPool(){
        super();
    }
    
    @Override
    protected Transformer create() {
        try {
            TransformerFactory tf = TransformerFactory.newInstance();
            Transformer trf = tf.newTransformer();
            return trf;
        } catch (TransformerConfigurationException ex) {
            Logger.getLogger(Util.class.getName()).log(Level.SEVERE, null, ex);
        } 
        return null;
    }

    @Override
    public Transformer acquire(){
        Transformer tra = (Transformer) super.acquire();
        tra.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");//NOI18N
        tra.setOutputProperty(OutputKeys.INDENT, "yes");//NOI18N
        //tra.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "4");
        tra.setOutputProperty(OutputKeys.ENCODING, "utf-8");//NOI18N        
        return tra;
    }
    
    public synchronized void release(Transformer o) {
        if (o != null){
            o.reset();
            super.release(o);
        }
    }
    
    public synchronized static TransformerPool getInstance(){
        if (instance == null){
            instance = new TransformerPool();
        }
        return instance;
    }
}
