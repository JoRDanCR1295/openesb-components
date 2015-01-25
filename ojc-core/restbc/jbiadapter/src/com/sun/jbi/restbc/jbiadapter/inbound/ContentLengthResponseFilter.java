package com.sun.jbi.restbc.jbiadapter.inbound;

import com.sun.jersey.spi.container.ContainerRequest;
import com.sun.jersey.spi.container.ContainerResponse;
import com.sun.jersey.spi.container.ContainerResponseFilter;

/**
 *
 * @author David BRASSELY (brasseld at gmail.com)
 * @author OpenESB Community
 */
public class ContentLengthResponseFilter implements ContainerResponseFilter {
    
    public ContainerResponse filter(ContainerRequest request, ContainerResponse response) {
        String contentLength = (String) response.getHttpHeaders().getFirst("X-Content-Length");
        if (contentLength != null) {
            response.getHttpHeaders().remove("Transfer-Encoding");
            response.getHttpHeaders().remove("X-Content-Length");
            response.getHttpHeaders().putSingle("Content-Length", contentLength);
        }

        return response;
    }
}
