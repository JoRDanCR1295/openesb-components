/**
 * Provides a custom classloading utility.
 * <p>
 * This utility is used to create a class loader that can load jar files from 
 * the path specified. For example, to load jar files packaged as part of a 
 * service unit, the service unit deployed root path is provided.</p>
 * <p>
 * The JBI spec requires that the Component be constructed (by the JBI 
 * implementation) using the execution class-loader. The component manager will 
 * create an instance of this class in its init() method passing in the 
 * execution class-loader. The execution class-loader will act as the parent of 
 * the service class-loader which delegates first to the parent and then 
 * attempts to load the jar's itself.</p>
 * <p>
 * Each component should have an instance of this utility. (Those who use 
 * <a href="http://wiki.open-esb.java.net/Wiki.jsp?page=ComponentToolkit">Component Toolkit</a> 
 * get it for free via the 
 * {@link com.sun.jbi.component.toolkit.lifecycle.ManagerContext ManagerContext}.)
 * <h2>Usage</h2>
 * The primary use case for this utility is to load user-provided jars which 
 * implement XPath extension functions for use in an XSL stylesheet. An internal 
 * cache of service class-loaders is maintained by service units deployed on the 
 * component. If there are jar files present in the service unit deployed path, 
 * which is registered during the ServiceUnitManager.init() method and 
 * unregistered during the ServiceUnitManager.shutdown() method.
 * <p>
 * Service unit operations may choose to switch the thread context class-loader 
 * of the calling thread by invoking the switchClassLoader() method. The 
 * component will cleanup this instance when its shutdown is called.</p>
 * <p>
 * <h4>Example: TransformServiceUnitManager</h4>
 * <code><pre>
 * public class TransformServiceUnitManager extends DefaultServiceUnitManager {
 *     public TransformServiceUnitManager(ManagerContext mgrCtx) {
 *         super(mgrCtx);
 *     }
 *     
 *     public void init(String serviceUnitName, String serviceUnitRootPath) throws DeploymentException {
 *         //initialize the service class loader.
 *         getManagerContext().getCustomClassLoaderUtil()
 *                 .registerServiceClassLoader(serviceUnitName, serviceUnitRootPath);
 *         super.init(serviceUnitName, serviceUnitRootPath);
 *     }
 *     
 *     public void shutDown(String serviceUnitName) throws DeploymentException {
 *         // remove any initialized service class-loader.
 *         getManagerContext().getCustomClassLoaderUtil()
 *                 .unregisterServiceClassLoader(serviceUnitName);
 *         super.shutDown(serviceUnitName);
 *     }
 * }</pre></code>
 */
package com.sun.jbi.common.classloader;