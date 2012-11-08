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
 * @(#)Xslt2Support.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.transform.api;

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.transform.Source;
import org.w3c.dom.Element;
import com.sun.jbi.common.util.Util;

/**
 * An <code>Xslt2Support</code> instance can be used to create 
 * {@link CompiledXslt2} and {@link Xslt2} instances, which are analogous to
 * {@link javax.xml.transform.Templates} and {@link javax.xml.transform.Transformer} types.
 * <p>
 * The system property that determines which <code>Xslt2Support</code> 
 * implementation to create is named &quot;com.sun.transform.api.Xslt2Support&quot;. 
 * This property names a concrete subclass of the <code>Xslt2Support</code> 
 * abstract class. 
 * <p>
 * If the property is not defined, a platform default is used which <b>MAY</b>
 * require additional jars in the classpath.
 * 
 * @author Kevan Simpson
 */
public abstract class Xslt2Support {

    public interface CompiledXslt2 {
        public Xslt2 newTransformer() throws Exception;
        public void cleanup();
    }

    public interface Xslt2 {
        public void setParam(String name, Object param) throws Exception;
        public Element doTransform(Source payload) throws Exception;
    }
    
    private static Logger mLogger = Logger.getLogger(Xslt2Support.class.getName());
    
    protected Xslt2Support() {
    }
    
    protected Logger log() {
        return mLogger;
    }
    
    public abstract CompiledXslt2 compile(Source xsl);
    
    public static Xslt2Support newInstance() throws Xslt2ConfigurationError {
        try {
            return (Xslt2Support) Util.find(
            /* The default property name as defined in this class */
            "com.sun.transform.api.Xslt2Support",
            /* The fallback implementation class name, which requires Saxon */
            "com.sun.transform.api.saxon.SaxonSupport",
            /* Pass this class to make its classloader available, if needed */
            Xslt2Support.class);
        } 
        catch (Xslt2ConfigurationError e) {
            throw error(e, I18n.loc(
                    "TRAPI-6001: Failed to create {0} instance: {1}", 
                    "Xslt2Support", e.getMessage()));
        }
        catch (Exception e) {
            throw error(e, I18n.loc(
                    "TRAPI-6001: Failed to create {0} instance: {1}", 
                    "Xslt2Support", e.getMessage()));
        }
    }

    protected static Xslt2ConfigurationError error(Throwable thrown, String msg) {
        if (thrown == null) {
            mLogger.warning(msg);
        }
        else {
            mLogger.log(Level.WARNING, msg, thrown);
        }
        
        if (thrown instanceof Xslt2ConfigurationError) {
            return (Xslt2ConfigurationError) thrown;
        }
        else if (thrown instanceof Exception) {
            return new Xslt2ConfigurationError((Exception) thrown, msg);
        }
        
        return new Xslt2ConfigurationError(msg);
    }
}
