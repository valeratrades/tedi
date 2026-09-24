## Tips
### Vim Fold Markers
Needs `foldmethod=marker` on markdown buffers.

Sprint views fold every multi-line component at level `base + depth` (base: milestone 1, category group 1, issue 3; depth: folded components around it). With `foldenable` and `foldlevel=2`, a sprint opens with its milestones and groups expanded and its issues closed.

Closed issues/sub-issues wrap their content in vim fold markers using `{{{always` suffix (nested one level deeper inside a sprint).
To auto-close these folds in nvim, add:
```lua
vim.opt.foldtext = [[substitute(getline(v:foldstart),'{{{]] .. [[always\s*$','{{{','')]] -- Custom foldtext that strips "always" from fold markers
vim.api.nvim_create_autocmd("BufReadPost", {
	callback = function()
		vim.defer_fn(function()
			vim.cmd([[silent! g/{{]] .. [[{always$/normal! zc]])
		end, 10)
	end,
})
```
