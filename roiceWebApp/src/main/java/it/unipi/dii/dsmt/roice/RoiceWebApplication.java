package it.unipi.dii.dsmt.roice;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.builder.SpringApplicationBuilder;
import org.springframework.boot.web.servlet.support.SpringBootServletInitializer;

@SpringBootApplication
public class RoiceWebApplication extends SpringBootServletInitializer {

    @Override
    protected SpringApplicationBuilder configure(SpringApplicationBuilder builder){
        return builder.sources(RoiceWebApplication.class);
    }

    public static void main(String[] args) {
        // Start-string
        SpringApplication.run(RoiceWebApplication.class, args);
    }

}
