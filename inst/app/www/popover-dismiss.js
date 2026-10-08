 // Dismiss popovers when clicking outside
$(document).ready(function() {
  $(document).on('click', function(e) {
    // Check if click is outside popover and its trigger button
    const $target = $(e.target);
    const isPopoverButton = $target.closest('.btn').parent().find('.popover').length > 0 ||
                            $target.closest('.btn').length > 0;
    const isInsidePopover = $target.closest('.popover').length > 0;

    if (!isPopoverButton && !isInsidePopover) {
      // Find and hide all visible popovers
      $('.popover').each(function() {
        const popoverId = $(this).attr('id');
        if (popoverId) {
          // Find the trigger element for this popover
          const $trigger = $('[aria-describedby="' + popoverId + '"]');
          if ($trigger.length > 0) {
            // Use Bootstrap's popover API to hide it
            const bsPopover = bootstrap.Popover.getInstance($trigger[0]);
            if (bsPopover) {
              bsPopover.hide();
            }
          }
        }
      });
    }
  });
});
