/**
 * Provides general utilities used throughout the common libraries.
 * <p>
 * <dl>
 *  <dt>AbstractPool</dt>
 *  <dd>Abstract implementation of a resource pool, where the templated type is 
 *      the type of pooled resource.</dd>
 *  <dt>Base64Utils</dt>
 *  <dd>Provides Base64 encoding and decoding methods.</dd>
 *  <dt>EntryRegistry</dt>
 *  <dd>Generic thread-safe registry, intended to be a mostly-read/rarely-update 
 *      utility. This is a multi-templated type, similar to {@link java.util.Map}, 
 *      with a key type and value type.</dd>
 *  <dt>LocalizationSupport</dt>
 *  <dd>A tool for obtaining localized messages. Components should extend this 
 *      class, following an existing 
 *      <a href="http://wiki.open-esb.java.net/Wiki.jsp?page=SystemicQualities.Logging.I18nUtility">I18n implementation</a> 
 *      as a model, to serve as a global i18n utility. Each component requires 
 *      one such implementation to indicate where the message bundle will reside.</dd>
 *  <dt>MBeanHelper</dt>
 *  <dd>Simple utility to assist in the registering and unregistering of MBeans.</dd>
 *  <dt>NDC</dt>
 *  <dd>Utility to enter and exit a nested diagnostic context. Instances of this 
 *      class can be used as an NDC marker, if for some reason the static 
 *      methods are insufficient.</dd>
 *  <dt>Util</dt>
 *  <dd>A static utility class with numerous methods.</dd>
 * </dl> 
 */
package com.sun.jbi.common.util;