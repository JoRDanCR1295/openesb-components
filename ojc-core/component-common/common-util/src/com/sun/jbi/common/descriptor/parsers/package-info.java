/**
 * Provides parsers for the various constructs in JBI descriptors.
 * <p>
 * There are several versions of JBI descriptors (service unit, service 
 * assembly, component), each with a corresponding parser. These parsers are not 
 * meant to be used directly; instead, use the corresponding 
 * {@link com.sun.jbi.common.descriptor.JbiDescriptor JbiDescriptor} utilities.<br><br>
 * <b>ServicesParser</b><br>
 * Parses a service unit descriptor.<br><br>
 * <b>ServiceAssemblyParser</b><br>
 * Parses a service assembly descriptor.<br><br>
 * <b><a href="http://wiki.open-esb.java.net/Wiki.jsp?page=QoS">QoS</a> Parsers</b><br>
 * <dl>
 *  <dt>AppConfigParser</dt>
 *  <dd>Parses application configuration and variables.</dd>
 *  <dt>QosConnectionParser</dt>
 *  <dd>Parses the connection extension elements in a service assembly 
 *      descriptor which contains systemic quality configurations.</dd>
 *  <dt>RedeliveryParser</dt>
 *  <dd>Parses redelivery configuration in a service assembly connection.</dd>
 *  <dt>ThrottlingParser</dt>
 *  <dd>Parses throttling configuration in a service assembly connection.</dd>
 *  <dt>TrackingParser</dt>
 *  <dd>Parses message tracking configuration in a service assembly connection.</dd>
 * </dl> 
 */
package com.sun.jbi.common.descriptor.parsers;