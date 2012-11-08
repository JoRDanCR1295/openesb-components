/**
 * Provides default XSLT runtime execution using JDK 
 * {@link javax.xml.transform} api. 
 * <p>
 * Please review the 
 * <a href="http://wiki.open-esb.java.net/Wiki.jsp?page=TransformActivityTSL">Transform activity</a>
 * wiki page for more information.
 * 
 * @see com.sun.transform.engine.runtime.function.InvokeFunction#invoke(Object, String, org.w3c.dom.Node)
 * @see com.sun.transform.engine.runtime.function.InvokeFunction#getNMProperty(Object, String)
 * @see com.sun.transform.engine.runtime.function.InvokeFunction#getNMProperty(Object, String, String)
 * @see com.sun.transform.engine.runtime.function.InvokeFunction#setNMProperty(Object, String, String)
 * @see com.sun.transform.engine.runtime.function.InvokeFunction#setNMProperty(Object, String, String, String)
 */
package com.sun.transform.engine.xslt;