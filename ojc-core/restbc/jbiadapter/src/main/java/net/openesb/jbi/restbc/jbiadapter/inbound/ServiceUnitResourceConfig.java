package net.openesb.jbi.restbc.jbiadapter.inbound;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Collection;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.ws.rs.container.ContainerRequestContext;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.ResponseBuilder;

import net.openesb.jbi.restbc.jbiadapter.I18n;
import net.openesb.jbi.restbc.jbiadapter.InboundConfiguration;
import net.openesb.jbi.restbc.jbiadapter.ServiceUnit;

import org.glassfish.jersey.process.Inflector;
import org.glassfish.jersey.server.ResourceConfig;
import org.glassfish.jersey.server.model.Resource;
import org.glassfish.jersey.server.model.ResourceMethod;

/**
 * Service resource for service units.
 *
 * @author Loic DASSONVILLE (ldassonville at gmail.com)
 * @author David BRASSELY (brasseld at gmail.com)
 * @author OpenESB Community
 *
 */
public class ServiceUnitResourceConfig extends ResourceConfig {

    /*
     * 101-110
     */
    private final static Logger logger = Logger.getLogger(ServiceUnitResourceConfig.class.getName());

    /**
     * Generate a JBI REST resource configuration for the given Service Unit.
     *
     * @param serviceUnit ServiceUnit to generate resource configuration.
     *
     */
    public ServiceUnitResourceConfig(ServiceUnit serviceUnit) {
        Collection<InboundConfiguration> configurations = serviceUnit.getInboundsConfigurations();

        for (InboundConfiguration inboundConfiguration : configurations) {
            final Resource resource = build(inboundConfiguration);
            registerResources(resource);
        }
    }

    private Resource build(InboundConfiguration inboundConfiguration) {
        final Resource.Builder resourceBuilder = Resource.builder();
        resourceBuilder.path(inboundConfiguration.getPathTemplate().getTemplate());

        final ResourceMethod.Builder methodBuilder = resourceBuilder.addMethod(inboundConfiguration.getMethod());
        methodBuilder.produces(inboundConfiguration.getProduceMediaTypes())
                .handledBy(new Inflector<ContainerRequestContext, Response>() {

                    @Override
                    public Response apply(ContainerRequestContext containerRequestContext) {

                        if (logger.isLoggable(Level.FINEST)) {
                            StringBuilder sb = new StringBuilder();
                            sb.append("\n");
                            sb.append("  URI: ").append(containerRequestContext.getUriInfo().getRequestUri().toString()).append("\n");
                            sb.append("  Method: ").append(containerRequestContext.getMethod()).append("\n");
                            sb.append("  Headers: ").append(containerRequestContext.getHeaders()).append("\n");

                            String msg = I18n.lf("RESTBC-1101: Inbound Request: {0}", sb.toString());//NOI18N
                            logger.finest(msg);
                        }

                        InboundDelegator inboundDelegator = InboundDelegator.getInstance();
                        if (inboundDelegator == null) {
                            String msg = I18n.loc("RESTBC-7101: Inbound delegator not initialized yet");
                            logger.severe(msg);
                            ResponseBuilder responseBuilder = Response.serverError();
                            responseBuilder.entity(msg);
                            return responseBuilder.build();
                        }

                        try {
                            ResponseBuilder responseBuilder = inboundDelegator.delegateRequest(containerRequestContext);
                            return responseBuilder.build();
                        } catch (Exception e) {

                            StringWriter sw = new StringWriter();
                            e.printStackTrace(new PrintWriter(sw));
                            //TODO
                            String msg = I18n.lf("RESTBC-???: Inbound Request error: {0}", sw.toString());//NOI18N
                            logger.severe(msg);
                            return Response.serverError().entity(msg).build();
                        }
                    }
                });
        
        return resourceBuilder.build();
    }
}
