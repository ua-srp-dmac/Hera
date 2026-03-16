FROM dmac/r-base:latest

# Ensure rapache module is loaded (r-base has mod_R.so but may lack the conf)
RUN echo "LoadModule R_module /usr/lib/apache2/modules/mod_R.so" \
    > /etc/apache2/conf-available/Rapache.conf \
    && a2enconf Rapache 2>/dev/null || true

# envsubst for templating Apache config with env vars at startup
RUN apt-get update && apt-get install -y --no-install-recommends gettext-base \
    && rm -rf /var/lib/apt/lists/*

# Create mod_auth_openidc session cache directory
RUN mkdir -p /var/cache/apache2/mod_auth_openidc \
    && chown www-data:www-data /var/cache/apache2/mod_auth_openidc

# Copy Hera R scripts
COPY HeraMaster.r /var/www/Rfiles/HeraMaster.r
COPY HeraDevelop.r /var/www/Rfiles/HeraDevelop.r
COPY HeraScripts/ /var/www/Rfiles/HeraScripts/

# Apache config template (env vars substituted at runtime)
COPY apache-hera.conf /etc/apache2/sites-available/hera.conf.template
RUN a2dissite 000-default

# Entrypoint script
COPY docker-entrypoint.sh /docker-entrypoint.sh
RUN chmod +x /docker-entrypoint.sh

EXPOSE 8080

CMD ["/docker-entrypoint.sh"]
