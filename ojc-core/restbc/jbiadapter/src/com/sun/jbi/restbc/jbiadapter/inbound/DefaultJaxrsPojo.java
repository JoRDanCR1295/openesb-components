package com.sun.jbi.restbc.jbiadapter.inbound;

import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;

/**
 * DefaultJaxrsPojo.java
 *
 * @author Edward Chou
 */
@Path("/welcome")
public class DefaultJaxrsPojo {
    
    @GET
    public String get() {
        return "Welcome to sample JAXRS POJO.";
    }
    
    @POST
    public String post() {
        return "Welcome to sample JAXRS POJO.";
    }
    
    
}
