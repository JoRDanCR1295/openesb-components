package net.openesb.jbi.restbc.jbiadapter;

/**
 * NMProps.java
 *
 * @author Edward Chou
 */
public final class NMProps {
    
    // NM Properties
    
    // request
    public final static String NM_URL_PROP = "org.glassfish.openesb.rest.url";
    public final static String NM_METHOD_PROP = "org.glassfish.openesb.rest.method";
    public final static String NM_CONTENT_TYPE_PROP = "org.glassfish.openesb.rest.content-type";
    public final static String NM_ACCEPT_TYPES_PROP = "org.glassfish.openesb.rest.accept-types";
    public final static String NM_ACCEPT_LANGUAGES_PROP = "org.glassfish.openesb.rest.accept-languages";
    public final static String NM_DATE_PROP = "org.glassfish.openesb.rest.date";
    public final static String NM_HEADERS_PROP = "org.glassfish.openesb.rest.headers";
    public final static String NM_PARAM_STYLE_PROP = "org.glassfish.openesb.rest.param-style";
    public final static String NM_PARAMS_PROP = "org.glassfish.openesb.rest.params";
    public final static String NM_PATH_PARAMS_PROP = "org.glassfish.openesb.rest.path-params";
    public final static String NM_BASICAUTH_USERNAME_PROP = "org.glassfish.openesb.rest.basic-auth-username";
    public final static String NM_BASICAUTH_PASSWORD_PROP = "org.glassfish.openesb.rest.basic-auth-password";
    
    // response
    public final static String NM_RESPONSE_STATUS_PROP = "org.glassfish.openesb.rest.response.status";
    public final static String NM_RESPONSE_URL_PROP = "org.glassfish.openesb.rest.response.url";
    public final static String NM_RESPONSE_CONTENT_TYPE_PROP = "org.glassfish.openesb.rest.response.content-type";
    public final static String NM_RESPONSE_HEADERS_PROP = "org.glassfish.openesb.rest.response.headers";
    
    // NM Properties related to payload
    public final static String NM_ENTITY_PROPS = "org.glassfish.openesb.rest.entity";
    
}
