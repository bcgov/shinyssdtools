// Client-side translation handler for shinyssdtools
$(document).ready(function() {
  
  // Custom message handler for translation updates
  Shiny.addCustomMessageHandler('updateTranslations', function(data) {
    const { translations, language } = data;
    
    // Update all elements with data-translate attributes
    $('[data-translate]').each(function() {
      const key = $(this).attr('data-translate');
      const $element = $(this);
      
      // Skip elements that are inside Shiny outputs to avoid breaking bindings
      if ($element.closest('.shiny-plot-output, .shiny-html-output, .shiny-text-output, .shiny-image-output, .datatables').length > 0) {
        return; // Skip this element
      }
      
      const iconHtml = $element.find('.bi').length > 0 ? $element.find('.bi')[0].outerHTML : '';
      
      if (translations[key]) {
        if (iconHtml) {
          // Preserve icon and add translated text with spacing
          $element.html(iconHtml + '<span style="margin-left: 0.5rem;">' + translations[key] + '</span>');
        } else {
          // Just update text content
          $element.html(translations[key]);
        }
      }
    });
    
    // Update language-specific attributes
    $('body').attr('data-language', language.toLowerCase());
    
    console.log('Translations updated for language:', language);
  });
  
  // Initialize with default language indicator
  $('body').attr('data-language', 'english');
});