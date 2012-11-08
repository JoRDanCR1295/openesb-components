/**
 * Provides an XML resource pool and static utility class.
 * <p>
 * Because it is expensive to create Transformer instances and DOM 
 * DocumentBuilders, it is helpful to allocate a pool to avoid unnecessary 
 * creation of these resources. Each 
 * {@link com.sun.jbi.common.xml.XmlResource XmlResource} instance contains a 
 * {@link javax.xml.transform.Transformer Transformer} and 
 * {@link javax.xml.parsers.DocumentBuilder DocumentBuilder}. Note, the default 
 * implementation of {@link com.sun.jbi.common.xml.XmlResourcePool XmlResourcePool} 
 * is an extension of Common-Util's 
 * {@link com.sun.jbi.common.util.AbstractPool AbstractPool} utility.
 * <h4>Usage</h4>
 * Components can either use the 
 * {@link com.sun.jbi.common.xml.XmlResourcePool XmlResourcePool} provided by 
 * the {@link com.sun.jbi.common.xml.XmlUtil XmlUtil} class or they can create 
 * their own. If the latter option is chosen, the component's pool can be set on 
 * the {@link com.sun.jbi.common.xml.XmlUtil XmlUtil} class to provide easy 
 * access to the pool within the component.
 * <h4>Example - Acquiring/Releasing XmlResource</h4>
 * <code><pre>
 *     // copied from com.sun.jbi.common.xml.XmlUtil
 *     public static Document newDocument() {
 *         XmlResource xml = null;
 *         try {
 *             xml = getXmlResourcePool().acquireXmlResource();
 *             return xml.getDocumentBuilder().newDocument();
 *         }
 *         finally {
 *             getXmlResourcePool().releaseXmlResource(xml);
 *         }
 *     }
 * </pre></code>
 */
package com.sun.jbi.common.xml;