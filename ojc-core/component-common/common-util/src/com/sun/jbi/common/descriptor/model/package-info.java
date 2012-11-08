/**
 * Defines models for various constructs in JBI descriptors.
 * <p>
 * <b>EndpointInfo</b><br>
 * Represents an endpoint configuration, both 'consumer' and 'provider' in a 
 * service assembly 'connection' as well as the 'consumes' and 'provides' 
 * entries in a service unit descriptor. This class can be used as a key in a 
 * {@link java.util.Map} to store endpoint-specific data in your component.<br><br>
 * <b>ServiceUnit</b><br>
 * Models a service unit descriptor, providing access to the unit's name, 
 * deployment root path, and its services (provides and consumes, together and 
 * separately).<br><br>
 * <b>ServiceAssembly</b><br>
 * Models a service assembly descriptor, providing access to the assembly's 
 * name, description, connections, and service units.<br><br>
 * <b>Supporting models</b><br>
 * <dl>
 *  <dt>AssemblyUnit</dt>
 *  <dd>A service unit deployed as part of a service assembly.</dd>
 *  <dt>Connection</dt>
 *  <dd>Links a consumer endpoint to a provider endpoint.</dd>
 *  <dt>Identification</dt>
 *  <dd>The name and description of a JBI construct.</dd>
 *  <dt>Services</dt>
 *  <dd>A list of endpoint configuration entries in a service unit descriptor.</dd>
 *  <dt>Target</dt>
 *  <dd>Models a deployable artifact and the component to which it is deployed.</dd>
 * </dl> 
 */
package com.sun.jbi.common.descriptor.model;