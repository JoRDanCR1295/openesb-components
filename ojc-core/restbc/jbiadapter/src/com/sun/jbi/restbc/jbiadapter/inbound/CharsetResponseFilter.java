package com.sun.jbi.restbc.jbiadapter.inbound;

import com.sun.jersey.spi.container.ContainerRequest;
import com.sun.jersey.spi.container.ContainerResponse;
import com.sun.jersey.spi.container.ContainerResponseFilter;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;

/**
 * 
 * @author David BRASSELY (brasseld at gmail.com)
 * @author OpenESB Community
 */
public class CharsetResponseFilter implements ContainerResponseFilter {
    
    public ContainerResponse filter(ContainerRequest request, ContainerResponse response) {

        MediaType contentType = (MediaType) response.getHttpHeaders().get(HttpHeaders.CONTENT_TYPE).iterator().next();
        response.getHttpHeaders().putSingle("Content-Type", contentType.toString() + ";charset=UTF-8");

        return response;
    }
}
